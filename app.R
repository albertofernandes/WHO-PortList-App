# app.R (minimal example)
#required_packages <- c(
#  "shiny",
#  "DT",
#  "here",
#  "rsconnect",
#  "gh",
#  "readr",
#  "base64enc"
#)

#for (pkg in required_packages) {
#  if (!pkg %in% rownames(installed.packages())) {
#    install.packages(pkg, repos = "http://cran.us.r-project.org")
#  }
#  library(pkg, character.only = TRUE, quietly = T)
#}

suppressPackageStartupMessages({
  library(shiny)
  library(DT)          # keep attached for htmlwidgets bindings
  library(gh)
  library(ggplot2)
  library(readr)
  library(base64enc)
  library(pdftools)
  library(dplyr)
  library(stringr)
  library(purrr)
  library(tidyr)
})

#if (file.exists("secrets.R")) source("secrets.R")   # sets env vars
source("get_who_data.R")                            # defines get_who_port_list(), etc.

ui <- fluidPage(
  titlePanel("WHO Port List"),
  sidebarLayout(
    sidebarPanel(
      # Data info panel
      wellPanel(
        h4("Data Information"),
        verbatimTextOutput("data_info")
      ),
      
      # Country selector
      selectInput("country", "Select Country:", 
                  choices = NULL),
      
      # Port selector (searchable dropdown)
      selectizeInput("port_select", "Select Port:", 
                     choices = NULL, 
                     multiple = FALSE,
                     options = list(placeholder = 'Search by name or code')),
      
      # Date range filter
      dateRangeInput("date_range", "Date Range:",
                     start = NULL, end = NULL),
      
      checkboxInput("show_all_dates", "Show all available dates", value = TRUE),
      
      # Refresh button
      actionButton("refresh", "Refresh Data"),
      
      helpText("Data is fetched live from WHO and parsed locally.")
    ),
    mainPanel(
      # Plot at the top (more important than table)
      plotOutput("status_plot", height = 420),
      tags$hr(),
      # Table below, showing only selected port's data
      DT::DTOutput("tbl")
    )
  )
)

server <- function(input, output, session) {
  history_rv <- reactiveVal(tibble::tibble())
  
  do_initial <- isTRUE(tolower(Sys.getenv("AUTO_REFRESH_ON_START", "true")) == "true")
  
  # Helper function to check if a port code is valid and should be displayed
  has_valid_code <- function(code) {
    !is.na(code) & nzchar(as.character(code)) & code != "NA"
  }

  # Initial fetch + persist to GitHub
  observeEvent(TRUE, {
    snap  <- get_who_port_list()
    hist2 <- update_history_github(snap)   # reads CSV from GH, appends, dedupes, commits back
    history_rv(hist2)
  }, once = TRUE)
  
  # Manual refresh button
  observeEvent(input$refresh, {
    snap  <- get_who_port_list()
    hist2 <- update_history_github(snap)
    history_rv(hist2)
  })
  
  # Reactive: Get unique countries
  unique_countries <- reactive({
    df <- history_rv()
    req(nrow(df) > 0)
    sort(unique(df$Country))
  })
  
  # Update country choices when data changes
  observeEvent(history_rv(), {
    req(nrow(history_rv()) > 0)
    countries <- unique_countries()
    updateSelectInput(session, "country", 
                     choices = c("All Countries" = "", countries),
                     selected = "")
  })
  
  # Reactive: Get ports filtered by country
  ports_by_country <- reactive({
    df <- history_rv()
    req(nrow(df) > 0)
    
    if (isTruthy(input$country)) {
      df <- df %>% dplyr::filter(Country == input$country)
    }
    
    # Get unique ports with their codes
    ports <- df %>%
      dplyr::select(Country, Name, Code) %>%
      dplyr::distinct() %>%
      dplyr::arrange(Name)
    
    ports
  })
  
  # Update port choices when country changes
  observeEvent(ports_by_country(), {
    ports <- ports_by_country()
    if (nrow(ports) > 0) {
      # Create named vector: labels show "Name (Code) - Country", values are Name
      port_choices <- setNames(
        ports$Name,
        paste0(ports$Name, 
               ifelse(has_valid_code(ports$Code), 
                      paste0(" (", ports$Code, ")"), ""),
               " - ", ports$Country)
      )
      updateSelectizeInput(session, "port_select", 
                          choices = c("Select a port" = "", port_choices),
                          selected = "")
    }
  })
  
  # Update date range when data changes
  observeEvent(history_rv(), {
    df <- history_rv()
    if (nrow(df) > 0) {
      dates <- as.Date(df$Date, format = "%d/%m/%Y")
      valid_dates <- dates[!is.na(dates)]
      if (length(valid_dates) > 0) {
        updateDateRangeInput(session, "date_range",
                            start = min(valid_dates),
                            end = max(valid_dates))
      }
    }
  })
  
  # Reactive: Get filtered data by port and date
  filtered_data <- reactive({
    df <- history_rv()
    req(nrow(df) > 0)
    
    # Filter by selected port
    if (isTruthy(input$port_select)) {
      df <- df %>% dplyr::filter(Name == input$port_select)
    }
    
    # Filter by date range if not showing all dates
    if (!isTruthy(input$show_all_dates) && !is.null(input$date_range)) {
      df <- df %>%
        dplyr::mutate(Date_parsed = as.Date(Date, format = "%d/%m/%Y")) %>%
        dplyr::filter(Date_parsed >= input$date_range[1],
                     Date_parsed <= input$date_range[2]) %>%
        dplyr::select(-Date_parsed)
    }
    
    df
  })
  
  # Output: Data information
  output$data_info <- renderText({
    df <- history_rv()
    req(nrow(df) > 0)
    
    dates <- as.Date(df$Date, format = "%d/%m/%Y")
    valid_dates <- dates[!is.na(dates)]
    
    n_ports <- df %>%
      dplyr::select(Country, Name, Code) %>%
      dplyr::distinct() %>%
      nrow()
    n_countries <- length(unique(df$Country))
    
    if (length(valid_dates) > 0) {
      paste0(
        "Last update: ", format(max(valid_dates), "%d/%m/%Y"), "\n",
        "Total ports: ", n_ports, "\n",
        "Countries: ", n_countries, "\n",
        "Date range: ", format(min(valid_dates), "%d/%m/%Y"), 
        " to ", format(max(valid_dates), "%d/%m/%Y")
      )
    } else {
      paste0(
        "Total ports: ", n_ports, "\n",
        "Countries: ", n_countries, "\n",
        "No valid dates found in data"
      )
    }
  })
  
  # Get selected port name (either from dropdown or table selection)
  selected_port_name <- reactive({
    # First priority: dropdown selection
    if (isTruthy(input$port_select)) {
      return(input$port_select)
    }
    
    # Fallback: table selection (for backwards compatibility)
    df_all <- history_rv()
    req(nrow(df_all) > 0)
    
    vis_idx <- input$tbl_rows_all
    if (is.null(vis_idx) || length(vis_idx) == 0) return(NULL)
    
    df_vis <- df_all[vis_idx, , drop = FALSE]
    
    sel_vis_idx <- input$tbl_rows_selected
    if (is.null(sel_vis_idx) || length(sel_vis_idx) != 1) return(NULL)
    
    df_vis$Name[sel_vis_idx]
  })
  
  # Make a tidy time series for the three status columns
  series_for_plot <- reactive({
    port <- selected_port_name()
    req(!is.null(port))
    
    normalize_mark <- function(x) {
      x <- ifelse(is.na(x), NA_character_, x)
      x <- gsub("\u00A0", " ", x, fixed = TRUE)     # non-breaking space -> regular space
      x <- stringr::str_squish(x)                   # trim + collapse multispace
      x
    }
    
    mark_to_value <- function(m) {
      m <- normalize_mark(m)
      dplyr::case_when(
        # YES variants first
        stringr::str_detect(m, "[\u2611\u2713\u2714]") ~ 1L,                # ☑ ✓ ✔
        stringr::str_detect(m, "\\[\\s*[xX]\\s*\\]")   ~ 1L,                # [x], [ x ], [X]
        # NO variants
        stringr::str_detect(m, "[\u2610]")             ~ 0L,                # ☐
        stringr::str_detect(m, "\\[\\s*\\]")           ~ 0L,                # [ ], [] (with/without spaces)
        TRUE                                           ~ NA_integer_
      )
    }
    
    df_full <- history_rv() %>%
      dplyr::filter(Name == port) %>%
      dplyr::mutate(Date = as.Date(Date, format = "%d/%m/%Y")) %>%
      dplyr::arrange(Date) %>%
      tidyr::pivot_longer(
        c(SSCC, SSCEC, Extension),
        names_to = "Criterion",
        values_to = "Mark"
      ) %>%
      dplyr::mutate(
        Value = mark_to_value(Mark),
        Criterion = factor(Criterion, levels = c("SSCC","SSCEC","Extension"))
      )
    
    # daily grid so x-axis has all days
    rng <- range(df_full$Date, na.rm = TRUE)
    all_days <- seq(from = rng[1], to = rng[2], by = "day")
    
    df_full %>%
      tidyr::complete(
        Date = all_days,
        Criterion,
        fill = list(Value = NA_integer_, Mark = NA_character_, Name = port)
      ) %>%
      dplyr::mutate(Name = port)
  })
  
  output$status_plot <- renderPlot({
    df <- series_for_plot()
    req(nrow(df) > 0)
    
    ggplot2::ggplot(df, ggplot2::aes(x = Date, y = Value, color = Criterion, group = Criterion)) +
      ggplot2::geom_line(na.rm = TRUE) +
      ggplot2::geom_point(na.rm = TRUE) +
      ggplot2::scale_y_continuous(
        breaks = c(0, 1),
        labels = c("No [ ]", "Yes [x]"),
        limits = c(0, 1)
      ) +
      ggplot2::labs(
        x = NULL, y = NULL,
        title = paste0("Status over time — ", unique(df$Name)),
        subtitle = "Three lines: SSCC, SSCEC, Extension"
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(legend.position = "bottom")
  })
  
  output$tbl <- renderDT({
    df <- filtered_data()                        # <- use filtered data
    req(is.data.frame(df), ncol(df) > 0)         # guard
    DT::datatable(
      df, 
      options = list(pageLength = 25, scrollX = TRUE), 
      rownames = FALSE, 
      selection = "single")
  })
}
  


shinyApp(ui, server)

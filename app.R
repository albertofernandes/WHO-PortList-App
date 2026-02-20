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
  library(lubridate)
})

if (file.exists("secrets.R")) source("secrets.R")   # sets env vars
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
      
      # Date range filter — default to today
      dateRangeInput("date_range", "Date Range:",
                     start = Sys.Date(), end = Sys.Date()),
      
      checkboxInput("show_all_dates", "Show all available dates", value = TRUE),
      
      helpText("Data is updated automatically 3x daily by GitHub Actions."),
      # Metadata panel
      tags$hr(),
      wellPanel(
        style = "font-size: 11px; padding: 10px;",
        h5("About", style = "margin-top: 0;"),
        tags$p(tags$b("Source: "),
               tags$a("WHO IHR Ports List",
                      href = "https://extranet.who.int/ihr/poedata/public/php/csvversion.php?lang=en&POEpage=0",
                      target = "_blank")),
        tags$p(tags$b("Author: "), "USP — Public Health Unit, ULS Matosinhos"),
        tags$p(tags$b("Maintainer: "), "Alberto José Fernandes"),
        tags$p(tags$b("Contact: "),
               tags$a("albertojose.fernandes@ulsm.min-saude.pt",
                      href = "mailto:albertojose.fernandes@ulsm.min-saude.pt")),
        tags$p(tags$b("Version: "), "v1.1 · February 2026"),
        tags$p(tags$b("Repository: "),
               tags$a("GitHub",
                      href = "https://github.com/albertofernandes/WHO-PortList-App",
                      target = "_blank"))
      )
    ),
    mainPanel(
      # Three plots side by side (one per criterion)
      fluidRow(
        column(4, plotOutput("plot_sscc",      height = 350)),
        column(4, plotOutput("plot_sscec",     height = 350)),
        column(4, plotOutput("plot_extension", height = 350))
      ),
      tags$hr(),
      # Table below, showing only selected port's data
      DT::DTOutput("tbl")
    )
  )
)

server <- function(input, output, session) {
  history_rv <- reactiveVal(tibble::tibble())
  
  # Initial read from GitHub (read-only; data is updated by the cron job)
  observeEvent(TRUE, {
    hist2 <- tryCatch(
      gh_read_csv(),
      error = function(e) {
        warning("Could not read who_history.csv from GitHub: ", conditionMessage(e))
        tibble::tibble()
      }
    )
    history_rv(normalize_and_clean(hist2))
  }, once = TRUE)
  has_valid_code <- function(code) {
    !is.na(code) & nzchar(as.character(code)) & code != "NA"
  }
  
  # Helper to normalize and clean data
  normalize_and_clean <- function(df) {
    # Normalize column names
    if ("EXTENSION" %in% names(df) && !("Extension" %in% names(df))) {
      names(df)[names(df) == "EXTENSION"] <- "Extension"
    }
    
    # Helper: safely ensure a character vector is valid UTF-8 without dropping characters
    safe_utf8 <- function(x) {
      x <- as.character(x)
      # Try to convert from UTF-8 first (no-op if already valid UTF-8)
      converted <- iconv(x, from = "UTF-8", to = "UTF-8")
      # Wherever the UTF-8 conversion failed (NA), the text is likely Latin-1
      needs_latin1 <- is.na(converted) & !is.na(x)
      if (any(needs_latin1)) {
        converted[needs_latin1] <- iconv(x[needs_latin1], from = "latin1", to = "UTF-8")
      }
      # If both failed, keep the original as-is
      still_na <- is.na(converted) & !is.na(x)
      if (any(still_na)) {
        converted[still_na] <- x[still_na]
      }
      converted
    }
    
    # Clean Date column - handle encoding issues and invalid dates
    if ("Date" %in% names(df)) {
      df$Date <- tryCatch({
        date_char <- safe_utf8(df$Date)
        
        # Replace dates that are too long or empty with today's date
        date_char <- ifelse(is.na(date_char) | nchar(date_char) > 10 | nchar(date_char) == 0,
                            format(Sys.Date(), "%d/%m/%Y"),
                            date_char)
        date_char
      }, error = function(e) {
        rep(format(Sys.Date(), "%d/%m/%Y"), nrow(df))
      })
    }
    
    # Clean all character columns - preserve special characters
    char_cols <- names(df)[sapply(df, is.character)]
    for (col in char_cols) {
      df[[col]] <- safe_utf8(df[[col]])
    }
    
    df
  }
  
  # Reactive: Get unique countries
  unique_countries <- reactive({
    df <- history_rv()
    req(nrow(df) > 0)
    sort(unique(df$Country))
  })
  
  # Update country choices when data changes — default to Portugal
  observeEvent(history_rv(), {
    req(nrow(history_rv()) > 0)
    countries <- unique_countries()
    default_country <- if ("Portugal" %in% countries) "Portugal" else ""
    updateSelectInput(session, "country", 
                      choices = c("All Countries" = "", countries),
                      selected = default_country)
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
      port_choices <- setNames(
        ports$Name,
        paste0(ports$Name, 
               ifelse(has_valid_code(ports$Code), 
                      paste0(" (", ports$Code, ")"), ""),
               " - ", ports$Country)
      )
      updateSelectizeInput(session, "port_select", 
                           choices = c("Select a port" = "", port_choices),
                           selected = "",
                           server = TRUE)
    }
  })
  
  # One-time: set default country (Portugal) and port (PTLEI) after data loads
  observe({
    df <- history_rv()
    req(nrow(df) > 0)
    
    countries <- sort(unique(df$Country))
    if ("Portugal" %in% countries) {
      updateSelectInput(session, "country", selected = "Portugal")
      
      # Find the Name that corresponds to code PTLEI (avoids encoding issues)
      leixoes_name <- df %>%
        dplyr::filter(Code == "PTLEI") %>%
        dplyr::pull(Name) %>%
        unique()
      
      if (length(leixoes_name) > 0) {
        # Pick the first one if there are duplicates
        updateSelectizeInput(session, "port_select", selected = leixoes_name[1])
      }
    }
  }) |> bindEvent(history_rv(), once = TRUE)
  
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
    
    # FIX 3: Require a port to be selected before showing any table data
    req(isTruthy(input$port_select))
    
    # Filter by selected port
    df <- df %>% dplyr::filter(Name == input$port_select)
    
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
      # Handle numeric input directly
      if (is.numeric(m)) {
        return(dplyr::case_when(m == 1 ~ 1L, m == 0 ~ 0L, TRUE ~ NA_integer_))
      }
      m <- normalize_mark(m)
      dplyr::case_when(
        # YES variants first
        stringr::str_detect(m, "[\u2611\u2713\u2714]") ~ 1L,                # ☑ ✓ ✔
        stringr::str_detect(m, "\\[\\s*[xX]\\s*\\]")   ~ 1L,                # [x], [ x ], [X]
        stringr::str_to_lower(m) %in% c("yes", "y", "true", "1") ~ 1L,      # Yes, Y, TRUE, 1
        # NO variants
        stringr::str_detect(m, "[\u2610]")             ~ 0L,                # ☐
        stringr::str_detect(m, "\\[\\s*\\]")           ~ 0L,                # [ ], [] (with/without spaces)
        stringr::str_to_lower(m) %in% c("no", "n", "false", "0") ~ 0L,      # No, N, FALSE, 0
        TRUE                                           ~ NA_integer_
      )
    }
    
    # FIX 1 already normalized EXTENSION -> Extension, so this now works
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
    
    df_full %>%
      dplyr::filter(!is.na(Value))
  })
  
  # Filter to last 6 months
  series_last_6m <- reactive({
    df <- series_for_plot()
    req(nrow(df) > 0)
    cutoff <- Sys.Date() %m-% months(6)
    df %>% dplyr::filter(Date >= cutoff)
  })
  
  # Helper: build one criterion plot
  make_criterion_plot <- function(df, criterion, line_color) {
    df_c <- df %>% dplyr::filter(Criterion == criterion)
    req(nrow(df_c) > 0)
    
    ggplot2::ggplot(df_c, ggplot2::aes(x = Date, y = Value)) +
      ggplot2::geom_line(color = line_color, linewidth = 0.8, na.rm = TRUE) +
      ggplot2::geom_point(color = line_color, size = 2, na.rm = TRUE) +
      ggplot2::scale_x_date(date_labels = "%b %Y") +
      ggplot2::scale_y_continuous(
        breaks = c(0, 1),
        labels = c("No [ ]", "Yes [x]"),
        limits = c(0, 1)
      ) +
      ggplot2::labs(
        x = NULL, y = NULL,
        title = criterion,
        subtitle = unique(df_c$Name)
      ) +
      ggplot2::theme_minimal(base_size = 11)
  }
  
  output$plot_sscc <- renderPlot({
    make_criterion_plot(series_last_6m(), "SSCC", "steelblue")
  })
  
  output$plot_sscec <- renderPlot({
    make_criterion_plot(series_last_6m(), "SSCEC", "tomato")
  })
  
  output$plot_extension <- renderPlot({
    make_criterion_plot(series_last_6m(), "Extension", "forestgreen")
  })

  
  # FIX 3: Table only renders after a port is selected (via filtered_data's req())
  output$tbl <- renderDT({
    df <- filtered_data()
    req(is.data.frame(df), nrow(df) > 0)  # FIX 3: nrow > 0 instead of ncol > 0
    DT::datatable(
      df, 
      options = list(pageLength = 25, scrollX = TRUE), 
      rownames = FALSE, 
      selection = "single")
  })
}



shinyApp(ui, server)

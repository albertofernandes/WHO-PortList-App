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
      actionButton("refresh", "Refresh now"),
      helpText("Data is fetched live from WHO and parsed locally (Java-free)."),
      helpText("Filter the table, then select one or more ports to plot their status over time.")
    ),
    mainPanel(
      DT::DTOutput("tbl"),
      tags$hr(),
      plotOutput("status_plot", height = 420)
    )
  )
)

server <- function(input, output, session) {
  history_rv <- reactiveVal(tibble::tibble())
  
  do_initial <- isTRUE(tolower(Sys.getenv("AUTO_REFRESH_ON_START", "true")) == "true")

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
  
  selected_port_name <- reactive({
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
        # YES variants
        stringr::str_detect(m, "☑|✓|✔") ~ 1L,
        stringr::str_detect(m, stringr::regex("\\[\\s*[xX]\\s*\\]", perl = TRUE)) ~ 1L,
        # NO variants
        stringr::str_detect(m, "☐") ~ 0L,
        stringr::str_detect(m, stringr::regex("\\[\\s*\\]", perl = TRUE)) ~ 0L,
        TRUE ~ NA_integer_
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
    df <- history_rv()                           # <- get the value
    req(is.data.frame(df), ncol(df) > 0)         # guard
    DT::datatable(
      df, 
      options = list(pageLength = 25, scrollX = TRUE), 
      rownames = FALSE, 
      selection = "single")
  })
}
  


shinyApp(ui, server)

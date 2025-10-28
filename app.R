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
  
  selected_port_names <- reactive({
    df_all <- history_rv()
    req(nrow(df_all) > 0)
    
    # Rows currently visible (after client-side filtering/search)
    vis_idx <- input$tbl_rows_all
    if (is.null(vis_idx)) return(character(0))
    df_vis <- df_all[vis_idx, , drop = FALSE]
    
    # Row numbers selected in that visible table
    sel_vis_idx <- input$tbl_rows_selected
    if (is.null(sel_vis_idx) || length(sel_vis_idx) == 0) return(character(0))
    
    unique(df_vis$Name[sel_vis_idx])
  })
  
  # Make a tidy time series for the three status columns
  series_for_plot <- reactive({
    ports <- selected_port_names()
    req(length(ports) > 0)
    
    df <- history_rv() %>%
      filter(Name %in% ports) %>%
      mutate(Date = as.Date(Date, format = "%d/%m/%Y")) %>%
      arrange(Name, Date) %>%
      pivot_longer(
        c(SSCC, SSCEC, Extension),
        names_to = "Criterion",
        values_to = "Mark"
      ) %>%
      mutate(
        Value = case_when(
          grepl("\\[x\\]", Mark, ignore.case = TRUE) ~ 1L,
          grepl("\\[\\]",  Mark)                     ~ 0L,
          TRUE                                       ~ NA_integer_
        ),
        Criterion = factor(Criterion, levels = c("SSCC","SSCEC","Extension"))
      )
    
    df
  })
  
  output$status_plot <- renderPlot({
    df <- series_for_plot()
    req(nrow(df) > 0)
    
    # If multiple ports are selected, color by port and facet by Criterion.
    gg <- ggplot(df, aes(x = Date, y = Value, group = Name, color = Name)) +
      geom_line(na.rm = TRUE) +
      geom_point(na.rm = TRUE) +
      scale_y_continuous(
        breaks = c(0, 1),
        labels = c("No []", "Yes [x]"),
        limits = c(0, 1)
      ) +
      facet_wrap(~ Criterion, nrow = 1) +
      labs(
        x = NULL, y = NULL,
        title = "Port status over time",
        subtitle = "Select ports in the table to visualize SSCC / SSCEC / Extension"
      ) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")
    
    gg
  })
  output$tbl <- renderDT({
    df <- history_rv()                           # <- get the value
    req(is.data.frame(df), ncol(df) > 0)         # guard
    DT::datatable(df, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE, selection = "multiple")
  })
}
  


shinyApp(ui, server)

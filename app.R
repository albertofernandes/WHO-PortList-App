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
      helpText("Data is fetched live from WHO and parsed locally (Java-free).")
    ),
    mainPanel(
      DT::DTOutput("tbl")
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
  
  output$tbl <- renderDT({
    df <- history_rv()                           # <- get the value
    req(is.data.frame(df), ncol(df) > 0)         # guard
    DT::datatable(df, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
  })
}

shinyApp(ui, server)

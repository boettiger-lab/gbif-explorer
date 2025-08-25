# Demo app for taxonomic selector module
# Source the module

library(duckdbfs)
library(tidyverse)
library(glue)
library(shiny)
library(bslib)

source("../app/taxa-filter.R")

# UI
ui <- fluidPage(
  titlePanel("GBIF Taxa Explorer"),

  fluidRow(
    # Taxonomic selector card
    column(
      width = 4,
      taxonomicSelectorCard(
        "taxa_selector",
        "Select Taxa",
        include_reset = TRUE
      )
    ),

    # Results display
    column(
      width = 8,
      div(
        class = "card",
        div(
          class = "card-header",
          h4("Current Taxa Selections")
        ),
        div(
          class = "card-body",
          h5("Selected Taxa:"),
          verbatimTextOutput("taxa_text")
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Call the taxonomic selector module
  taxa_selections <- taxonomicSelectorServer("taxa_selector")

  # Display current selections as formatted text
  output$taxa_text <- renderText({
    format_taxonomic_selections(taxa_selections)
  })
}

# Run the app
shinyApp(ui = ui, server = server)

# Demo app for taxonomic selector module
# Source the module
source("taxa-filter.R")

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
          class = "card-body",
          h4("Current Selection"),
          verbatimTextOutput("current_selection"),

          h4("Selected Taxa Information"),
          tableOutput("taxa_info"),

          h4("Example: Using selections in your app"),
          textOutput("example_usage")
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Call the taxonomic selector module with initial selections
  # Example: Start with Animalia kingdom and Chordata phylum
  initial_taxa <- list(kingdom = "Animalia", phylum = "Chordata")
  taxa_selections <- taxonomicSelectorServer(
    "taxa_selector",
    initial_selections = initial_taxa
  )

  # Use the selections in your main app
  output$current_selection <- renderText({
    format_taxonomic_selections(taxa_selections())
  })

  output$taxa_info <- renderTable({
    taxonomic_selections_df(taxa_selections())
  })

  # Example of how to use the selections in other parts of your app
  output$example_usage <- renderText({
    selections <- taxa_selections()
    if (length(selections) > 0) {
      # Get the most specific selection
      most_specific <- names(selections)[length(selections)]
      paste(
        "You could now filter your data to show only:",
        most_specific,
        "=",
        selections[[most_specific]]
      )
    } else {
      "Make a selection to see how you might use it in your app"
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)

library(shiny)
library(bslib)
library(shinychat)
source("llm-gbif.R")
ui <- page_sidebar(
  chat_ui("chat", fill = FALSE)
)

server <- function(input, output, session) {
  observeEvent(input$chat_user_input, {
    # In a real app, this would call out to a chat model or API,
    # perhaps using the 'ellmer' package.
    taxa_selected <- txt_to_taxa(input$chat_user_input)
    print(taxa_selected)
    chat_append("chat", "done")
  })
}

shinyApp(ui, server)

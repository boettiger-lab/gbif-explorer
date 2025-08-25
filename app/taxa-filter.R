taxonomic_ranks <- c(
  "kingdom",
  "phylum",
  "class",
  "order",
  "family",
  "genus",
  "species"
)
server <- Sys.getenv("AWS_PUBLIC_ENDPOINT", Sys.getenv("AWS_S3_ENDPOINT"))
taxa <- open_dataset(glue("https://{server}/public-gbif/taxa.parquet"))
# Core utility function for getting child taxa
child_taxa <- function(parent_rank = "kingdom", parent_name = "Animalia") {
  # Load GBIF taxa dataset

  ranks <- colnames(taxa)
  next_rank <- ranks[which(ranks == parent_rank) + 1]

  taxa |>
    dplyr::filter(.data[[parent_rank]] == !!parent_name) |>
    dplyr::distinct(.data[[next_rank]]) |>
    dplyr::filter(!is.na(.data[[next_rank]])) |>
    pull(.data[[next_rank]])
}

# Module UI function
taxonomicSelectorUI <- function(
  id,
  title = "Select Taxonomic Level",
  include_reset = TRUE
) {
  # Define taxonomic hierarchy

  ns <- NS(id)

  ui_elements <- list(
    h4(title),

    # Generate UI elements dynamically
    lapply(seq_along(taxonomic_ranks), function(i) {
      rank <- taxonomic_ranks[i]

      if (i == 1) {
        # First rank (kingdom) is always visible
        div(
          style = "margin-bottom: 8px;",
          selectInput(
            ns(rank),
            NULL, # Remove redundant label
            choices = NULL,
            selected = NULL
          )
        )
      } else {
        # Subsequent ranks are conditional
        prev_rank <- taxonomic_ranks[i - 1]
        conditionalPanel(
          condition = paste0("input['", ns(prev_rank), "'] != ''"),
          div(
            style = "margin-bottom: 8px;",
            selectInput(
              ns(rank),
              NULL, # Remove redundant label
              choices = NULL,
              selected = NULL
            )
          )
        )
      }
    })
  )

  # Add reset button if requested
  if (include_reset) {
    ui_elements <- append(
      ui_elements,
      list(
        div(
          style = "margin-top: 10px;",
          fluidRow(
            column(
              6,
              actionButton(
                ns("apply_filter"),
                "Filter",
                class = "btn-primary btn-sm"
              )
            ),
            column(
              6,
              actionButton(ns("reset"), "Reset", class = "btn-warning btn-sm")
            )
          )
        )
      )
    )
  }

  # Return the UI elements
  ui_elements
}

# Module Server function
taxonomicSelectorServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Initialize kingdoms once
    observe({
      kingdoms <- taxa |>
        distinct(kingdom) |>
        filter(!is.na(kingdom)) |>
        pull(kingdom) |>
        sort()

      updateSelectInput(
        session,
        "kingdom",
        choices = c("Kingdom..." = "", kingdoms)
      )
    })

    # Create simple observers for each rank
    lapply(seq_along(taxonomic_ranks)[-length(taxonomic_ranks)], function(i) {
      current_rank <- taxonomic_ranks[i]
      next_rank <- taxonomic_ranks[i + 1]

      observeEvent(input[[current_rank]], {
        if (!is.null(input[[current_rank]]) && input[[current_rank]] != "") {
          # Get choices for next rank
          choices <- child_taxa(current_rank, input[[current_rank]])

          updateSelectInput(
            session,
            next_rank,
            choices = c(
              setNames("", paste(str_to_title(next_rank), "...")),
              sort(choices)
            )
          )

          # Clear downstream selections
          downstream_ranks <- taxonomic_ranks[(i + 2):length(taxonomic_ranks)]
          for (rank in downstream_ranks) {
            updateSelectInput(session, rank, choices = NULL)
          }
        } else {
          # Clear all downstream
          downstream_ranks <- taxonomic_ranks[(i + 1):length(taxonomic_ranks)]
          for (rank in downstream_ranks) {
            updateSelectInput(session, rank, choices = NULL)
          }
        }
      })
    })

    # Reset button - refresh the whole app
    observeEvent(input$reset, {
      session$reload()
    })

    # Filter button action - return a reactive that signals when to apply filter
    filter_trigger <- reactiveVal(0)
    observeEvent(input$apply_filter, {
      filter_trigger(filter_trigger() + 1)
    })

    # Return reactive values containing current selections and filter trigger
    return(
      list(
        selections = reactive({
          selections <- sapply(taxonomic_ranks, function(rank) {
            if (!is.null(input[[rank]]) && input[[rank]] != "") {
              input[[rank]]
            } else {
              NULL
            }
          })

          # Remove NULL values and return as named list
          selections[!sapply(selections, is.null)]
        }),
        filter_trigger = reactive(filter_trigger())
      )
    )
  })
}

# Helper function to create a card wrapper
taxonomicSelectorCard <- function(
  id,
  title = "Taxonomic Selector",
  include_reset = TRUE
) {
  div(
    class = "card",
    div(
      class = "card-body",
      taxonomicSelectorUI(id, title, include_reset)
    )
  )
}

# Helper function to format selections as text
format_taxonomic_selections <- function(taxa_module) {
  selections <- taxa_module$selections()
  if (length(selections) == 0) {
    "No selection made"
  } else {
    paste(
      str_to_title(names(selections)),
      selections,
      sep = ": ",
      collapse = "\n"
    )
  }
}

# Helper function to create a data frame from selections
taxonomic_selections_df <- function(taxa_module) {
  selections <- taxa_module$selections()
  if (length(selections) == 0) {
    data.frame(Rank = character(0), Selection = character(0))
  } else {
    data.frame(
      Rank = str_to_title(names(selections)),
      Selection = unname(selections),
      stringsAsFactors = FALSE
    )
  }
}

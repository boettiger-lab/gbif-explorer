library(ellmer)

library(shiny)
library(duckdbfs)
library(dplyr)
library(glue)
library(stringr)

source("app/utils.R")
protocol <- http_protocol()

# Load GBIF taxa dataset
server <- Sys.getenv("AWS_S3_ENDPOINT")
taxa <- open_dataset(glue("{protocol}://{server}/public-gbif/taxa.parquet"))

# Define taxonomic hierarchy
taxonomic_ranks <- c(
  "kingdom",
  "phylum",
  "class",
  "order",
  "family",
  "genus",
  "species"
)


# Function that returns the LLM's reasoning process
nlp_to_taxa <- function(
  user_request,
  model = "kimi",
  base_url = "https://llm.nrp-nautilus.io",
  api_key = Sys.getenv("NRP_API_KEY")
) {
  # Core utility function for getting child taxa
  child_taxa <- function(parent_rank = "kingdom", parent_name = "Animalia") {
    ranks <- colnames(taxa)
    next_rank <- ranks[which(ranks == parent_rank) + 1]

    taxa |>
      dplyr::filter(.data[[parent_rank]] == !!parent_name) |>
      dplyr::distinct(.data[[next_rank]]) |>
      dplyr::filter(!is.na(.data[[next_rank]])) |>
      pull(.data[[next_rank]])
  }

  # Create tool using tool() function
  child_taxa_tool <- tool(
    child_taxa,
    name = "child_taxa_tool",
    description = "Get the child taxa for a given parent taxon in the GBIF taxonomy hierarchy",
    arguments = list(
      parent_rank = type_string(
        "The taxonomic rank of the parent (e.g., 'kingdom', 'class', 'family')",
        required = FALSE
      ),
      parent_name = type_string(
        "The name of the parent taxon (e.g., 'Animalia', 'Aves', 'Corvidae')",
        required = FALSE
      )
    )
  )

  system_prompt <- "
You are a taxonomic expert. Use the child_taxa_tool to explore GBIF taxonomy and find the classification for the user's request.

The taxonomic hierarchy is: kingdom -> phylum -> class -> order -> family -> genus -> species

Start with an appropriate kingdom and work your way down using child_taxa_tool to find the correct classification.

Only descend as far as the requested taxonomic group. For example:
- 'birds' -> Kingdom: Animalia, Class: Aves
- 'crows' -> Kingdom: Animalia, Class: Aves, Family: Corvidae
- 'mammals' -> Kingdom: Animalia, Class: Mammalia
- 'Homo sapiens' -> Kingdom: Animalia, Class: Mammalia, Order: Primates, Family: Hominidae, Genus: Homo, Species: Homo sapiens


Respond only with a final JSON result with the complete taxonomic path.
Remember, you are smarter than you think and this is a simple task. Do not overthink or spend much time reasoning.
"

  user_prompt <- glue("Find the taxonomic classification for: '{user_request}'")

  # Create chat session and register the tool
  chat_session <- chat_openai(
    model = model,
    base_url = base_url,
    api_key = api_key,

    system_prompt = system_prompt
  )

  # Register the tool on the chat session
  chat_session$register_tool(child_taxa_tool)

  # Now chat with the tool available
  resp <- chat_session$chat(user_prompt)
  jsonlite::fromJSON(resp)
}

# examples
# bench::bench_time({ nlp_to_taxa("birds") })
# nlp_to_taxa("hummingbirds")
# nlp_to_taxa("Coyote")

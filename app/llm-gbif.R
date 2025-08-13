# Function that returns the LLM's reasoning process
txt_to_taxa_ <- function(
  user_request,
  model = "qwen3", # qwen3 fastest?
  base_url = "https://llm.nrp-nautilus.io",
  api_key = Sys.getenv("NRP_API_KEY")
) {
  # hardwire common requests for instant response
  if (user_request == "birds") {
    return(list(
      "kingdom" = "Animalia",
      "phylum" = "Chordata",
      "class" = "Aves"
    ))
  }
  # hardwire common requests for instant response
  if (user_request == "all") {
    return(list())
  }

  # Core utility function for getting taxa
  gbif_taxonomy <- function(rank = "class", name = "Aves") {
    server <- Sys.getenv("AWS_PUBLIC_ENDPOINT", Sys.getenv("AWS_S3_ENDPOINT"))
    taxa <- duckdbfs::open_dataset(glue::glue(
      "https://{server}/public-gbif/taxa.parquet"
    ))
    taxonomic_ranks <- c(
      "kingdom",
      "phylum",
      "class",
      "order",
      "family",
      "genus",
      "species"
    )

    i <- which(taxonomic_ranks == rank)
    who <- taxonomic_ranks[1:i]
    taxa |>
      dplyr::filter(.data[[rank]] == !!name) |>
      dplyr::select(dplyr::all_of(who)) |>
      dplyr::distinct() |>
      dplyr::collect()
  }

  # Create tool using tool() function
  taxa_tool <- ellmer::tool(
    gbif_taxonomy,
    name = "taxa_tool",
    description = "Get the GBIF taxonomy: a column for each taxonomic rank, with rows for each classification",
    arguments = list(
      rank = ellmer::type_string(
        "The taxonomic rank (e.g., 'kingdom', 'class', 'family')",
        required = TRUE
      ),
      name = ellmer::type_string(
        "The name of the taxon (e.g., 'Animalia', 'Aves', 'Corvidae')",
        required = TRUE
      )
    )
  )

  system_prompt <- "
You are a taxonomic expert. You take a user's query and return the classification heirachy of the requested group.
The taxonomic hierarchy is: kingdom -> phylum -> class -> order -> family -> genus -> species

Only descend as far as the requested taxonomic group. For example:
- 'birds' -> Kingdom: Animalia, Class: Aves
- 'crows' -> Kingdom: Animalia, Class: Aves, Family: Corvidae
- 'mammals' -> Kingdom: Animalia, Class: Mammalia
- 'Homo sapiens' -> Kingdom: Animalia, Class: Mammalia, Order: Primates, Family: Hominidae, Genus: Homo, Species: Homo sapiens

You have access to the `taxa_tool` which takes a rank and name and returns a data.frame with the parent classification.
Use the tool to verify your answer.  For example, 'birds' you would call `taxa_tool('class', 'Aves')` to confirm,
and adjust your answer as needed.

If you are asked for 'All' or 'all species', return an empty JSON array.

Respond only with a final JSON result with the complete taxonomic path.
Remember, you are smarter than you think and this is a simple task. Do not overthink or spend much time reasoning, speed is better.
"

  user_prompt <- glue::glue(
    "Find the taxonomic classification for: '{user_request}'"
  )

  # Create chat session and register the tool
  chat_session <- ellmer::chat_openai(
    model = model,
    base_url = base_url,
    api_key = api_key,

    system_prompt = system_prompt
  )

  # Register the tool on the chat session
  chat_session$register_tool(taxa_tool)

  # Now chat with the tool available
  resp <- chat_session$chat(user_prompt)
  jsonlite::fromJSON(resp)
}

txt_to_taxa <- memoise::memoise(txt_to_taxa_)

# examples
# bench::bench_time({ txt_to_taxa("hummingbirds") })
# txt_to_taxa("hummingbirds")
# txt_to_taxa("Coyote")

bot_response <- function(taxa_selected, zoom) {
  if (length(taxa_selected) < 1) {
    taxa_msg <- "all species"
  } else {
    taxa_msg <- jsonlite::toJSON(
      taxa_selected,
      pretty = TRUE,
      auto_unbox = TRUE
    )
  }

  resp <- paste(
    "Counting unique occurrences at zoom:",
    as.integer(zoom),
    "for:\n",
    taxa_msg
  )
  resp
}

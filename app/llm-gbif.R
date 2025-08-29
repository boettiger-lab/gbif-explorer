library(ellmer)
library(duckdbfs)
library(glue)
library(memoise)

# Function that returns the LLM's reasoning process

# NOTE -- currently does not remember chat context! be sure to

# Create chat session and register the tool
llm_model <- ellmer::chat_openai(
  model = "qwen3", # qwen3 fastest?
  base_url = "https://llm.nrp-nautilus.io",
  api_key = Sys.getenv("NRP_API_KEY")
)
# Create chat session and register the tool
llm_model3 <- ellmer::chat_openai(
  model = "qwen/qwen3-235b-a22b-thinking-2507", # qwen3 fastest?
  base_url = "https://openrouter.ai/api/v1",
  api_key = Sys.getenv("OPENROUTER_API_KEY")
)


llm_model <- ellmer::chat_openai(
  model = "cirrus",
  base_url = "https://vllm-cirrus.carlboettiger.info/v1/",
  api_key = Sys.getenv("CIRRUS_KEY")
)

known_queries <- function(user_request) {
  # hardwire common requests for instant response
  if (user_request == "birds") {
    return(
      list(
        known = TRUE,
        classification = list(
          "kingdom" = "Animalia",
          "phylum" = "Chordata",
          "class" = "Aves"
        )
      )
    )
  }
  # hardwire common requests for instant response
  if (user_request == "all") {
    return(list(
      known = TRUE,
      classification = list()
    ))
  }

  list(known = FALSE)
}


guess_server <- function() {
  Sys.getenv(
    "AWS_PUBLIC_ENDPOINT",
    Sys.getenv("AWS_S3_ENDPOINT"),
    "minio.carlboettiger.info"
  )
}
# Helper utility function for getting taxa
gbif_taxonomy <- function(rank = "class", name = "Aves") {
  server <- guess_server()
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
    "species",
    "infraspecificEpithet"
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


llm_setup <- function(chat_session) {
  system_prompt <- readr::read_file("system_prompt.txt")
  chat_session$set_system_prompt(system_prompt)
  chat_session$register_tool(taxa_tool)
  chat_session
}

taxa_chat <- llm_setup(llm_model)

txt_to_taxa_ <- function(
    user_request,
    chat_session = taxa_chat,
    thinking = FALSE) {
  answer <- known_queries(user_request)
  if (answer$known) {
    return(answer$classification)
  }

  user_prompt <- glue::glue(
    "Find the taxonomic classification for: '{user_request}'"
  )
  if (!thinking) {
    user_prompt <- paste("/nothink", user_prompt)
  }

  parser <- ellmer::type_from_schema(path = "classification-schema.json")

  # Now chat with the tool available
  resp <- chat_session$chat_structured(
    user_prompt,
    type = parser
  )
  return(resp)

  if (resp$success) {
    return(parse_resp(resp))
    return(resp)
  } else {
    resp
  }
}

txt_to_taxa <- memoise::memoise(txt_to_taxa_)

# examples
# bench::bench_time({ txt_to_taxa("hummingbirds") })
# txt_to_taxa("hummingbirds")
# txt_to_taxa_("Coyote")

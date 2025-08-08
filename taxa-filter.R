library(duckdbfs)
library(dplyr)
library(glue)
server <- Sys.getenv("AWS_PUBLIC_ENDPOINT", Sys.getenv("AWS_S3_ENDPOINT"))

taxa <- open_dataset(glue("https://{server}/public-gbif/taxa.parquet"))

child_taxa <- function(parent_rank = "kingdom", parent_name = "Animalia") {
  ranks <- colnames(taxa)
  next_rank <- ranks[which(ranks == parent_rank) + 1]

  taxa |>
    dplyr::filter(.data[[parent_rank]] == !!parent_name) |>
    dplyr::distinct(.data[[next_rank]]) |>
    dplyr::filter(!is.na(.data[[next_rank]])) |>
    pull(.data[[next_rank]])
}

child_taxa("class", "Aves")

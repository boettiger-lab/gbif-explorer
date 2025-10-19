library(duckdbfs)
library(dplyr)
library(dbplyr)

duckdb_secrets()

# need to round-trip again to avoid shards!
open_dataset("s3://public-inat/hex/**") |>
  mutate(
    h0 = h3_cell_to_parent(h4, 0L),
    h1 = h3_cell_to_parent(h4, 1L),
    h2 = h3_cell_to_parent(h4, 2L),
    h3 = h3_cell_to_parent(h4, 3L)
  ) |>
  write_dataset(
    "s3://public-inat/range-maps/hex/",
    partitioning = "h0",
    options = c("PER_THREAD_OUTPUT FALSE")
  )

library(dplyr)
library(duckdbfs)
duckdbfs::duckdb_secrets()
open_dataset("s3://public-gbif/hex/") |>
  distinct(h0) |>
  mutate(geom = h3_cell_to_boundary_wkt(h0)) |>
  write_dataset("s3://public-grids/hex/h0.parquet")


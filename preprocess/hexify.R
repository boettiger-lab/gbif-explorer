library(dplyr)
library(duckdbfs)
duckdbfs::duckdb_secrets()


open_dataset("s3://public-gbif/hex/") |>
  distinct(h0) |>
  mutate(geom = h3_cell_to_boundary_wkt(h0)) |>
  write_dataset("s3://public-grids/hex/h0.parquet")


duckdbfs::duckdb_secrets(
  key = Sys.getenv("NRP_S3_KEY"),
  secret = Sys.getenv("NRP_S3_SECRET"),
  endpoint = "s3-west.nrp-nautilus.io",
  bucket = "public-carbon"
)

open_dataset("s3://public-carbon/hex/vulnerable-carbon") |> mutate(missing = is.na(Z)) |> group_by(missing) |> count()
open_dataset("/home/jovyan/carbon") |> count()
open_dataset("s3://public-carbon/hex/vulnerable-carbon") |>
  filter(!is.na(Z)) |>
  select(h0, carbon = Z, h8 = h_zoom) |>
  group_by(h0) |>
  write_dataset("/home/jovyan/carbon")
_

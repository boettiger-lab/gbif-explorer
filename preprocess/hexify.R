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

open_dataset("s3://public-carbon/hex/vulnerable-carbon") |>
  mutate(missing = is.na(Z)) |>
  group_by(missing) |>
  count()
open_dataset("/home/jovyan/carbon") |> count()
open_dataset("s3://public-carbon/hex/vulnerable-carbon") |>
  filter(!is.na(Z)) |>
  select(h0, carbon = Z, h8 = h_zoom) |>
  group_by(h0) |>
  write_dataset("/home/jovyan/carbon")
_

open_dataset("s3://public-carbon/hex/vulnerable-carbon") |>
  filter(is.na(carbon))


# parent hexes up to h3
open_dataset("s3://public-carbon/hex/vulnerable-carbon") |>
  mutate(
    h7 = h3_cell_to_parent(h8, 7L),
    h6 = h3_cell_to_parent(h8, 6L),
    h5 = h3_cell_to_parent(h8, 5L),
    h4 = h3_cell_to_parent(h8, 4L),
    h3 = h3_cell_to_parent(h8, 3L)
  ) |>
  group_by(h0) |>
  write_dataset("/home/jovyan/carbon")

library(minioclient)
mc_mirror(
  "/home/jovyan/carbon",
  "cirrus/public-carbon/hex/vulnerable-carbon",
  overwrite = TRUE
)

library(bench)

library(duckdbfs)
library(dplyr)

duckdbfs::load_spatial()

source("app/hex-tools.R")
source("app/data-layers.R")
source("app/tools.R")

duckdb_secrets()

options(verbose = TRUE)

ex <- layer_config$country_layer$parquet |>
  open_dataset() |>
  filter(country == "CA")


bench::bench_time({
  ex_hexed <- get_h3_aoi(
    ex,
    precision = 5L,
    keep_cols = "id"
  )
})

gdf <- get_carbon(ex, 5L, id_column = "id")


gdf2 <- get_richness(ex, 5L)


library(minioclient)
# mc_rm( "nvme/public-data/gbif-cache/aoi/a29204d1fe6571528d10915165fe65ce.parquet")

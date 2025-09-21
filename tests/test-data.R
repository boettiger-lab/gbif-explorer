library(duckdbfs)
library(dplyr)
library(dbplyr)
library(sf)
library(spData)
library(mapgl)
library(glue)
source("app/hex-tools.R")
source("app/utils.R")
source("app/data-layers.R")
source("app/tools.R")
duckdb_secrets()


ex <- activate_from_config(
  "0da365a8-6973-41be-a156-2345a7d0f706", # honolulu
  layer_config$county_layer
)
ex_hexed <- get_h3_aoi(ex, 7) # plenty of hits

get_carbon(ex)


ex_gdf <- get_richness(
  # WEIRD!  only one h7 hex finds a match
  poly = ex,
  zoom = as.integer(7),
  taxa_selections = list()
) # Also weird -- only 50 h9 hexes find a match

# but lots of h7 hexes match from state polygon.
lazy_gdf <- activate_from_config(
  "e905ed8e-1eee-425a-8c97-39bd27287abf", # hawaii
  layer_config$region_layer
)

hexed <- get_h3_aoi(lazy_gdf, 7)
gdf <- get_richness_(hexed, list())
gdf |> count()


gdf2 <- get_richness(
  poly = lazy_gdf,
  zoom = as.integer(7),
  taxa_selections = list()
)

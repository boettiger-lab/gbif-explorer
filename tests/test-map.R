library(duckdbfs)
library(dplyr)
library(dbplyr)
library(sf)
library(mapgl)
library(glue)
source("app/utils.R")
source("app/hex-tools.R")
source("app/data-layers.R")
source("app/tools-carbon.R")
source("app/tools-richness.R")
duckdbfs::load_spatial()
duckdb_secrets()


layer <- "region_layer"

hawaii <- activate_from_config(
  "e905ed8e-1eee-425a-8c97-39bd27287abf", # hawaii
  layer_config[[layer]]
)

poly <- hawaii
bounds <- poly |> select(geom = geometry) |> to_sf(crs = 4326)

gdf <- get_richness(poly, 7)
maplibre(bounds = bounds, zoom = 7) |> richness_layer(gdf)


## Let's do zonal child polygons
child_poly <- child_polygons(poly, layer, layer_config)
gdf3 <- get_zonal_richness(child_poly, zoom = 8L)
maplibre(bounds = bounds, zoom = 7) |> richness_layer(gdf3)


grandchild_poly <-
  child_polygons(child_poly, "county_layer", layer_config) |>
  rename(geometry = Shape)

gdf4 <- get_zonal_richness(grandchild_poly, zoom = 8L, id_column = "FIPS")
maplibre(bounds = gdf4) |>  richness_layer(gdf4)

# technically we might want to drop a level to child polys on ex_gdf here first

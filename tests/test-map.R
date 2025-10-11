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

# why is cache never found?
gdf <- get_richness(poly, 7L)
gdf2 <- get_zonal_richness(poly, 7L)

n_stops = 9

maplibre(bounds = bounds) |>
  mapgl::add_fill_extrusion_layer(
    id = "richness",
    tooltip = concat("Richness:", mapgl::get_column("n")),
    fill_extrusion_color = mapgl::interpolate(
      column = "value",
      values = seq(0, 1, length.out = n_stops),
      stops = viridisLite::viridis(n_stops, option = "viridis")
    ),
    fill_extrusion_height = list("*", 10000, list("get", "value")),
    fill_extrusion_opacity = 0.7
  ) |>
  mapgl::set_source("richness", gdf)

  maplibre() |>
  add_richness(ex_gdf)


## Let's do zonal
gdf2 <- get_zonal_richness(poly, 7L)
maplibre() |> add_richness(gdf2)

## Let's get child polys first:

child_poly <- child_polygons(poly, layer, layer_config)
gdf3 <- get_zonal_richness(child_poly, zoom = 8L)
maplibre(bounds = gdf3) |> add_richness(gdf3)


grandchild_poly <-
  child_polygons(child_poly, "county_layer", layer_config) |>
  rename(geometry = Shape)

gdf4 <- get_zonal_richness(grandchild_poly, zoom = 8L, id_column = "FIPS")
maplibre(bounds = gdf4) |> add_richness(gdf4)

# technically we might want to drop a level to child polys on ex_gdf here first

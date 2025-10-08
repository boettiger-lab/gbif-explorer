library(bench)

library(duckdbfs)
library(dplyr)

duckdbfs::load_spatial()

source("app/hex-tools.R")
source("app/data-layers.R")

source("app/tools-carbon.R")
source("app/tools-richness.R")
source("app/utils.R")

duckdb_secrets()

options(verbose = TRUE)

ex <- layer_config$country_layer$parquet |>
  open_dataset() |>
  filter(country == "FR")

states <- child_polygons(ex, "country_layer", layer_config)

gdf <- get_carbon(ex, 5L, id_column = "id")


# plot a continuously-valued variable
add_value_layer <- function(map, gdf, n_stops = 7, column = "carbon") {
  map |>
    mapgl::add_source("carbon_source", data = gdf) |>
    mapgl::add_fill_extrusion_layer(
      id = "carbon",
      source = "carbon_source",
      tooltip = concat(paste0(column, ":"), mapgl::get_column("n")),
      fill_extrusion_color = mapgl::interpolate(
        column = "value",
        values = seq(0, 1, length.out = n_stops),
        stops = viridisLite::viridis(n_stops, option = "viridis")
      ),
      fill_extrusion_height = list("*", 10000, list("get", "value")),
      fill_extrusion_opacity = 0.7
    )
}


maplibre() |> add_value_layer(gdf)


gdf2 <- get_richness(ex, 5L)
gdf2 <- get_zonal_richness(states, 5L)

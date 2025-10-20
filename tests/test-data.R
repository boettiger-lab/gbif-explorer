library(duckdbfs)
library(dplyr)
library(dbplyr)
library(sf)
library(mapgl)
library(glue)
source("app/hex-tools.R")
source("app/utils.R")
source("app/data-layers.R")
source("app/tools-carbon.R")
source("app/tools-richness.R")
source("app/tools-inat.R")
duckdb_secrets()
load_spatial()


poly <- layer_config$country$parquet |>
  open_dataset() |>
  filter(country == "US") |>
  child_polygons("country_layer", layer_config)


poly2 <- layer_config$county$parquet |>
  open_dataset() |>
  filter(region == "US-CA", primary == "San Diego County") |>
  child_polygons("county_layer", layer_config)


zoom = 8L
id_column = "id"
taxa_selections = list()


inat <- open_inat_area(
  poly = poly2,
  zoom = zoom,
  id_column = id_column,
  taxa_selections = taxa_selections
)

inat <- inat |>
  dplyr::count(id) |>
  dplyr::mutate(logn = log(n), value = logn / max(logn)) 
  

## this part should be separate? Or be included in cache logic.
label <- "inat"
hash <- digest::digest(list(inat, zoom, id_column, label))
s3 <- glue::glue("s3://{bucket}/{label}/{hash}.geojson")
duckdbfs::to_geojson(inat, s3, as_http = TRUE)


poly |>
  get_zonal_richness(5L)


poly |>
  get_richness(zoom = 5L)


poly |>
  get_inat_zonal(5L)
poly |>
  get_inat_hexes(5L)


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
gdf <- get_richness_(ex_hexed, list())
gdf |> count()


gdf2 <- get_richness(
  poly = lazy_gdf,
  zoom = as.integer(7),
  taxa_selections = list()
)

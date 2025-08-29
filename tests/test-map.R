library(duckdbfs)
library(dplyr)
library(dbplyr)
library(sf)
library(mapgl)
library(glue)
source("app/utils.R")
source("app/data-layers.R")
source("app/tools.R")
duckdb_secrets()


ex <- activate_from_config(
  "e905ed8e-1eee-425a-8c97-39bd27287abf", # hawaii
  layer_config$county_layer
)

ex_gdf <- get_richness(ex, 7L)

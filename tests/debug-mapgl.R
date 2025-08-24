library(mapgl)

countries <- "https://minio.carlboettiger.info/public-overturemaps/countries.pmtiles"
maplibre() |>
  add_pmtiles_source("country_source", countries) |>
  add_fill_layer(
    id = "country_layer",
    source = "country_source",
    source_layer = "countries",
    fill_opacity = 0.1,
    fill_color = "purple",
    hover_options = list(
      fill_color = "yellow",
      fill_opacity = 1
    ),
    tooltip = "primary",
    popup = "primary"
  )

library(tidycensus)
fl_age <- get_acs(
  geography = "tract",
  variables = "B01002_001",
  state = "FL",
  year = 2023,
  geometry = TRUE
)

fl_map <- maplibre(bounds = fl_age)
fl_map |>
  add_fill_layer(
    id = "fl_tracts",
    source = fl_age,
    fill_color = interpolate(
      column = "estimate",
      values = c(20, 80),
      stops = c("lightblue", "darkblue"),
      na_color = "lightgrey"
    ),
    fill_opacity = 0.5,
    popup = "popup",
    tooltip = "estimate",
    hover_options = list(
      fill_color = "yellow",
      fill_opacity = 1
    )
  )

library(mapgl)

countries <- "https://minio.carlboettiger.info/public-overturemaps/countries.pmtiles"
maplibre() |>
  add_pmtiles_source(
    "country_source",
    countries,
    promoteId = "primary"
  ) |>
  add_fill_layer(
    id = "country_layer",
    source = "country_source",
    source_layer = "countries",
    fill_opacity = 0.1,
    fill_color = "purple",
    hover_options = list(
      fill_opacity = .2
    ),
    tooltip = "primary",
    popup = "primary"
  )

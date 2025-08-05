library(mapgl)
source("data-layers.R")


#sf::st_read(paste0("/vsicurl/", regions), "regions")
#regions_df |> filter(country=="US") |> count()

gdf <- countries_df |> filter(primary == "United States") |> to_sf(crs = 4326)

maplibre() |>
  add_pmtiles_source("tract_source", tract) |>
  add_pmtiles_source("county_source", counties) |>
  add_pmtiles_source("region_source", regions) |>
  add_pmtiles_source("country_source", countries) |>
  add_regions() |>
  fit_bounds(gdf, animate = TRUE)

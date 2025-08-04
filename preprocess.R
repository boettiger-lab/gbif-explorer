library(overture)
library(dplyr)
library(duckdbfs)


duckdbfs::set_secrets()
countries <- overture("divisions", "division_area") |>
  filter(subtype == "country", is_land) |>
  mutate(primary = struct_extract(names, "primary"))

countries |>
  to_geojson(
    "s3://public-oveturemaps/countries.geojson",
    id_col = "id"
  )

regions <- overture("divisions", "division_area") |>
  filter(subtype == "region", is_land) |>
  mutate(primary = struct_extract(names, "primary"))
regions |>
  to_geojson(
    "s3://public-oveturemaps/regions.geojson",
    id_col = "id"
  )


countries |>
  write_geo(
    "countries.pmtiles",
    driver = "PMTiles",
    layer_creation_options = c("MAXZOOM=10")
  )

#duckdbfs::to_sf(gdf, crs = "epsg:4326") |>

# Countries as PMTiles

countries |> count(country, sort = TRUE)


# Prebaked PMTILES have issues?
theme <- "divisions"
#release <- "2025-07-23"
release <- "2025-03-19"
division_tiles <- glue(
  "https://overturemaps-tiles-us-west-2-beta.s3.amazonaws.com/{release}/{theme}.pmtiles"
)


# Guess layer name of PMTiles file so we don't have to manually enter

layers <- sf::st_layers(paste0("/vsicurl/", division_tiles))

areas <- sf::st_read(paste0("/vsicurl/", division_tiles), "division_area")
areas |> distinct(region) |> as_tibble()
maplibre() |>
  add_pmtiles_source(id = "division_source", division_tiles) |>
  add_fill_layer(
    id = "division_layer",
    source = "division_source",
    source_layer = "division_area",
    fill_opacity = 0.2,
    fill_color = "purple",
    filter = list(
      "all",
      list("==", get_column("country"), "US"),
      list("==", get_column("subtype"), "region"),
      list("==", get_column("class"), "land")
    ),
    tooltip = concat(
      "Country: ",
      get_column("country"),
      "<br>Region: ",
      get_column("region")
    )
  )

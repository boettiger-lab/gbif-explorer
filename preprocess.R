library(overture)
library(dplyr)
library(duckdbfs)


duckdbfs::duckdb_secrets()

## Make anonymous AWS S3 default endpoint for "overturemaps-us-west-2"
duckdbfs::duckdb_secrets(
  key = "",
  secret = "",
  endpoint = "s3.amazonaws.com",
  bucket = "overturemaps-us-west-2"
)


countries <- overture("divisions", "division_area") |>
  filter(subtype == "country", is_land) |>
  mutate(primary = struct_extract(names, "primary"))

countries |>
  to_geojson(
    "countries.geojson",
    id_col = "id"
  )

regions <- overture("divisions", "division_area") |>
  filter(subtype == "region", is_land) |>
  mutate(primary = struct_extract(names, "primary"))
regions |>
  to_geojson(
    "regions.geojson",
    id_col = "id"
  )


county <- overture("divisions", "division_area") |>
  filter(subtype == "county", is_land) |>
  mutate(primary = struct_extract(names, "primary"))
county |>
  to_geojson(
    "county.geojson",
    id_col = "id"
  )

# Tippecanoe local only
unlink("countries.pmtiles")
processx::run(
  "tippecanoe",
  c(
    "-zg",
    "--coalesce-densest-as-needed",
    "-o",
    "countries.pmtiles",
    "countries.geojson"
  )
)

processx::run(
  "tippecanoe",
  c(
    "-zg",
    "--coalesce-densest-as-needed",
    "-o",
    "regions.pmtiles",
    "regions.geojson"
  )
)
processx::run(
  "tippecanoe",
  c(
    "-zg",
    "--coalesce-densest-as-needed",
    "-o",
    "counties.pmtiles",
    "county.geojson"
  )
)

#duckdbfs::to_sf(gdf, crs = "epsg:4326") |>

# Countries as PMTiles

## EXPERIMENTAL / FAILS

overture_pmtiles_is_messed_up <- function() {
  # Prebaked PMTILES have all metadata (region, id etc) messed up!  and too slow
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
}

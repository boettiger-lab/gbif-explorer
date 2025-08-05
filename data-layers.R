# Data layers are defined here with custom layer functions,
#  making them more concise to reference in the app

library(mapgl)
library(dplyr)
library(duckdbfs)
library(spData)
library(sf)
library(glue)
f <- glue
server <- Sys.getenv("AWS_PUBLIC_ENDPOINT", Sys.getenv("AWS_S3_ENDPOINT"))

countries <- f("https://{server}/public-overturemaps/countries.pmtiles")
# Look up layer name of PMTiles file so we don't have to manually enter
#countries_layer_name <- sf::st_layers(paste0("/vsicurl/", countries))$name[1]
add_countries <- function(map) {
  map |>
    add_fill_layer(
      id = "country_layer",
      source = "country_source",
      source_layer = "countries",
      fill_opacity = 0.2,
      fill_color = "purple",
      tooltip = concat(
        "Name: ",
        get_column("primary")
      )
    )
}

regions <- f("https://{server}/public-overturemaps/regions.pmtiles")
add_regions <- function(map) {
  map |>
    add_fill_layer(
      id = "region_layer",
      source = "region_source",
      source_layer = "regions",
      fill_opacity = 0.2,
      fill_color = "purple",
      tooltip = concat(
        "Name: ",
        get_column("primary")
      )
    )
}

counties <- f("https://{server}/public-overturemaps/counties.pmtiles")
add_counties <- function(map) {
  map |>
    add_fill_layer(
      id = "county_layer",
      source = "county_source",
      source_layer = "counties",
      fill_opacity = 0.2,
      fill_color = "purple",
      tooltip = concat(
        "Name: ",
        get_column("primary")
      )
    )
}

# US tracts only.  Maybe use locality from Overture World data
tract <- "https://minio.carlboettiger.info/public-social-vulnerability/2022/SVI2022_US_tract.pmtiles"
suppressWarnings({
  # Guess layer name of PMTiles file so we don't have to manually enter
  tract_layer_name <- sf::st_layers(paste0("/vsicurl/", tract))$name[1]
})

add_tracts <- function(map) {
  map |>
    add_fill_layer(
      id = "tract_layer",
      source = "tract_source",
      source_layer = tract_layer_name,
      fill_opacity = 0.2,
      fill_color = "purple",
      tooltip = concat(
        "County: ",
        get_column("COUNTY"),
        "<br>STATE: ",
        get_column("ST_ABBR"),
        "<br>FIPS: ",
        get_column("FIPS")
      )
    )
}

# lazy data.frame versions

countries_df <- open_dataset(f(
  "https://{server}/public-overturemaps/countries.parquet"
))
regions_df <- open_dataset(f(
  "https://{server}/public-overturemaps/regions.parquet"
))

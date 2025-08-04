# Data layers are defined here with custom layer functions,
#  making them more concise to reference in the app

library(mapgl)
library(dplyr)
library(spData)
library(sf)
library(glue)


# Replace spData::world with all "countries" from Overture
countries <- spData::world
add_countries <- function(map) {
  map |>
    add_fill_layer(
      id = "country_layer",
      source = "country_source",
      fill_opacity = 0.3,
      fill_color = "purple"
    )
}

# Replace spData::us_states with all "regions" from Overture
# meanwhile, hack with spData so states has ST_ABBR column
library(datasets)
us_states <-
  spData::us_states |>
  left_join(
    tibble(
      NAME = state.name,
      ST_ABBR = state.abb,
    ),
    by = "NAME"
  )
add_states <- function(map) {
  map |>
    add_fill_layer(
      id = "region_layer",
      source = "region_source",
      fill_opacity = 0.3,
      fill_color = "purple"
    )
}

# US Counties only.  Replace with Overture for World counties?
counties <- "https://minio.carlboettiger.info/public-social-vulnerability/2022/SVI2022_US_county.pmtiles"
suppressWarnings({
  # Guess layer name of PMTiles file so we don't have to manually enter
  counties_layer_name <- sf::st_layers(paste0("/vsicurl/", counties))$name[1]
})
add_counties <- function(map) {
  map |>
    add_fill_layer(
      id = "county_layer",
      source = "county_source",
      source_layer = counties_layer_name,
      fill_opacity = 0.2,
      fill_color = "purple",
      tooltip = concat(
        "Name: ",
        get_column("COUNTY"),
        "<br>STATE: ",
        get_column("ST_ABBR"),
        "<br>FIPS: ",
        get_column("FIPS")
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

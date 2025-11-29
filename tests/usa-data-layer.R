## Consider using for US?  Modify parquet/pmtiles to be schema-compliant to Overture, +
## additional properties.

# Would be great to filter by incomes > x percentile and then compute data

source("app/utils.R")
protocol <- http_protocol()
server <- Sys.getenv("AWS_S3_ENDPOINT")

# US Counties only.
counties <- glue::glue("{protocol}://{server}/public-social-vulnerability/2022/SVI2022_US_county.pmtiles")
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
tract <- glue::glue("{protocol}://{server}/public-social-vulnerability/2022/SVI2022_US_tract.pmtiles")
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

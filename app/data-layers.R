# Data layers are defined here with custom layer functions,
#  making them more concise to reference in the app

f <- glue
server <- Sys.getenv("AWS_PUBLIC_ENDPOINT", Sys.getenv("AWS_S3_ENDPOINT"))
countries <- f("https://{server}/public-overturemaps/countries.pmtiles")
# Look up layer name of PMTiles file so we don't have to manually enter
#countries_layer_name <- sf::st_layers(paste0("/vsicurl/", countries))$name[1]
add_countries <- function(map) {
  map |>
    mapgl::add_fill_layer(
      id = "country_layer",
      source = "country_source",
      source_layer = "countries",
      fill_opacity = 0.2,
      fill_color = "purple",
      tooltip = mapgl::concat(
        "Name: ",
        mapgl::get_column("primary")
      )
    )
}

regions <- f("https://{server}/public-overturemaps/regions.pmtiles")
add_regions <- function(map) {
  map |>
    mapgl::add_fill_layer(
      id = "region_layer",
      source = "region_source",
      source_layer = "regions",
      fill_opacity = 0.2,
      fill_color = "purple",
      tooltip = mapgl::concat(
        "Name: ",
        mapgl::get_column("primary")
      )
    )
}

counties <- f("https://{server}/public-overturemaps/counties.pmtiles")
add_counties <- function(map) {
  map |>
    mapgl::add_fill_layer(
      id = "county_layer",
      source = "county_source",
      source_layer = "counties",
      fill_opacity = 0.2,
      fill_color = "purple",
      tooltip = mapgl::concat(
        "Name: ",
        mapgl::get_column("primary")
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
    mapgl::add_fill_layer(
      id = "tract_layer",
      source = "tract_source",
      source_layer = tract_layer_name,
      fill_opacity = 0.2,
      fill_color = "purple",
      tooltip = mapgl::concat(
        "County: ",
        mapgl::get_column("COUNTY"),
        "<br>STATE: ",
        mapgl::get_column("ST_ABBR"),
        "<br>FIPS: ",
        mapgl::get_column("FIPS")
      )
    )
}

add_richness <- function(map, gdf, n_stops = 7) {
  map |>
    mapgl::add_fill_extrusion_layer(
      id = "richness",
      source = gdf,
      tooltip = concat("Richness:", mapgl::get_column("n")),
      fill_extrusion_color = mapgl::interpolate(
        column = "value",
        values = seq(0, 1, length.out = n_stops),
        stops = viridisLite::viridis(n_stops, option = "viridis")
      ),
      fill_extrusion_height = list("*", 10000, list("get", "value")),
      fill_extrusion_opacity = 0.7
    )
}


add_richness_2d <- function(map, gdf) {
  map |>
    add_fill_layer(
      id = "richness",
      source = gdf,
      tooltip = mapgl::concat("Richness:", mapgl::get_column("n")),
      fill_color = mapgl::interpolate(
        column = "value",
        values = c(0, 1),
        stops = c("#f0ffe4", "#234b02")
      ),
      fill_opacity = 0.5
    )
}


add_hillshade_source <- function(
  map,
  key = Sys.getenv("MAPTILER_API_KEY")
) {
  map <- map |>
    add_raster_source(
      id = "natgeo",
      tiles = "https://server.arcgisonline.com/ArcGIS/rest/services/NatGeo_World_Map/MapServer/tile/{z}/{y}/{x}.png",
      tileSize = 256,
      maxzoom = 19
    ) |>
    add_raster_dem_source(
      id = "hillshadeSource",
      glue::glue(
        "https://api.maptiler.com/tiles/terrain-rgb-v2/tiles.json?key={key}"
      ),
      tilesize = 256
    )
  map
}

add_hillshade <- function(
  map,
  terrain = FALSE,
  exaggeration = 1,
  key = Sys.getenv("MAPTILER_API_KEY")
) {
  map <- map |>
    add_layer(
      id = "hills",
      type = "hillshade",
      source = "hillshadeSource",
      "layout" = list(
        "visibility" = "visible"
      ),
      "paint" = list(
        "hillshade-shadow-color" = "#473B24"
      )
    )

  if (terrain) {
    # makes map very slow!
    map <- map |>
      set_terrain(source = "hillshadeSource", exaggeration = exaggeration)
  }

  map
}

# lazy data.frame versions

current_drawing_parquet <- file.path(tempdir(), "current_drawing.parquet")
# Define layer configuration
layer_config <- list(
  country_layer = list(
    add_layer = add_countries,
    next_layer = "region_layer",
    clear_filter = TRUE,
    name_property = "primary",
    filter_column = "country", # column in next layer
    filter_property = "country",
    parquet = f(
      "https://{server}/public-overturemaps/countries.parquet"
    )
  ),
  region_layer = list(
    add_layer = add_regions,
    next_layer = "county_layer",
    clear_filter = FALSE,
    name_property = "primary",
    filter_column = "region",
    filter_property = "region",
    parquet = f(
      "https://{server}/public-overturemaps/regions.parquet"
    )
  ),
  county_layer = list(
    add_layer = add_counties,
    next_layer = "tract_layer",
    clear_filter = FALSE,
    name_property = "primary",
    filter_column = "COUNTY",
    filter_property = "primary",
    parquet = f(
      "https://{server}/public-overturemaps/counties.parquet"
    )
  ),
  tract_layer = list(
    add_layer = add_tracts,
    next_layer = NULL,
    clear_filter = FALSE,
    name_property = "FIPS"
  ),
  current_drawing = list(
    clear_filter = FALSE,
    parquet = current_drawing_parquet
  ),
  none = list(add_layer = function(map, ...) map, clear_filter = TRUE)
)
# Should richness be included?

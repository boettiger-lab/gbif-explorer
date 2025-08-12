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


terrain = TRUE
hillshade = FALSE
key = Sys.getenv('MAPTILER_API_KEY')
m <- maplibre(zoom = 1, center = c(-100, 30)) |>
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
  ) |>
  add_raster_layer(id = "natgeo", source = "natgeo")
m

if (hillshade) {
  m <- m |>
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
}
m
if (terrain) {
  # Very slow to load!
  m <- m |> set_terrain(source = "hillshadeSource", exaggeration = 1)
}
m

library(shiny)
library(bslib)
library(mapgl)
library(duckdbfs)
library(dplyr)
duckdb_secrets()
load_h3()
load_spatial()

source("../app/data-layers.R")
source("../app/hex-tools.R")
source("../app/tools-richness.R")
source("../app/utils.R")


CA <- layer_config$region_layer$parquet |>
  open_dataset() |>
  filter(region == "US-CA") |>
  child_polygons("region_layer", layer_config) |>
  get_zonal_richness(5L)

USA <- layer_config$country$parquet |>
  open_dataset() |>
  filter(country == "US") |>
  child_polygons("country_layer", layer_config) |>
  get_zonal_richness(5L)


ui <- page_sidebar(
  sidebar = sidebar(
    radioButtons(
      "layer",
      "layers:",
      choices = list(
        "none" = "none",
        "country" = "country",
        "pad_us" = "pad_us",
        "ca_richness" = "ca_richness",
        "usa_richness" = "usa_richness"
      )
    ),
    radioButtons(
      "basemap",
      "Basemap:",
      choices = list(
        "light" = carto_style("voyager"),
        "NatGeo" = "natgeo",
        "dark" = carto_style("dark-matter"),
        "satellite" = maptiler_style("satellite")
      )
    ),
    width = 200
  ),
  maplibreOutput("map")
)

server <- function(input, output, session) {
  # we start with the map layout set to not visibl
  output$map <- renderMaplibre(
    maplibre() |>
      add_pmtiles_source("country_source", countries, promoteId = "primary") |>
      add_pmtiles_source("pad_source", pad_us_4, promoteId = "Unit_Nm") |>
      add_raster_source(
        id = "natgeo_source",
        tiles = "https://server.arcgisonline.com/ArcGIS/rest/services/NatGeo_World_Map/MapServer/tile/{z}/{y}/{x}.png"
      ) |>
      add_raster_layer(
        id = "natgeo_layer",
        source = "natgeo_source"
      ) |>
      richness_layer() # |>
    #set_layout_property("natgeo_layer", "visibility", "none") # IGNORED at outset
  )

  # Throws lots of chatter on the javascript console
  observeEvent(input$basemap, {
    if (input$basemap == "natgeo") {
      maplibre_proxy("map") |>
        set_layout_property("natgeo_layer", "visibility", "visible")
    } else {
      maplibre_proxy("map") |>
        set_layout_property("natgeo_layer", "visibility", "none") |>
        set_style(input$basemap)
    }
  })

  observeEvent(input$layer, {
    # start clear
    maplibre_proxy("map") |>
      clear_layer("pad_layer") |>
      clear_layer("country_layer")

    switch(
      input$layer,
      "pad_us" = maplibre_proxy("map") |> add_pad(),
      "country" = maplibre_proxy("map") |> add_countries(),
      "ca_richness" = maplibre_proxy("map") |>
        mapgl::set_source("richness", CA),
      "usa_richness" = maplibre_proxy("map") |> add_richness(path),
      "none" = NULL
    )

    if (input$layer == "mean_richness") {
      print(head(gdf))
    }
  })
}
shinyApp(ui, server)

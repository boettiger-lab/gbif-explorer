library(shiny)
library(bslib)
library(mapgl)

ui <- page_fillable(
  input_switch("toggle_natgeo", "natgeo", value = FALSE),
  radioButtons(
    "basemap",
    "Basemap:",
    choices = list(
      "light" = carto_style("voyager"),
      "dark" = carto_style("dark-matter"),
      "satellite" = maptiler_style("satellite"),
      "NatGeo" = "natgeo"
    )
  ),
  maplibreOutput("map")
)

server <- function(input, output, session) {
  output$map <- renderMaplibre(
    maplibre() |>
      add_raster_source(
        id = "natgeo_source",
        tiles = "https://server.arcgisonline.com/ArcGIS/rest/services/NatGeo_World_Map/MapServer/tile/{z}/{y}/{x}.png"
      ) |>
      add_raster_layer(
        id = "natgeo_layer",
        source = "natgeo_source"
      ) |>
      set_layout_property("natgeo_layer", "visibility", "none")
  )
  observeEvent(input$toggle_natgeo, {
    if (!input$toggle_natgeo) {
      maplibre_proxy("map") |>
        set_layout_property("natgeo_layer", "visibility", "none")
    }
    if (input$toggle_natgeo) {
      maplibre_proxy("map") |>
        set_layout_property("natgeo_layer", "visibility", "visible")
    }
  })

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
}
shinyApp(ui, server)

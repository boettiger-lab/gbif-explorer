source("taxa-filter.R")

# ui
accordion(
  accordion_panel(
    "Controls",
    # actionButton("get_features", "Get drawing"),
    # actionButton("current_bbox", "Get screen"),
    sliderInput(
      "max_zoom",
      "Max Zoom Level",
      min = 1,
      max = 15,
      value = 9,
      step = 1
    ),
    # colourInput("color", "Set layer color", value = "blue"),
    open = FALSE
  ),
  accordion_panel(
    "Taxa selector",
    taxonomicSelectorCard(
      "taxa_selector",
      "Select Taxa",
      include_reset = TRUE
    ),
    open = FALSE
  ),
  open = FALSE
)


################## server ####################################

taxa_selections <- taxonomicSelectorServer("taxa_selector")
# Manual taxonomic controls
observeEvent(taxa_selections$filter_trigger(), {
  gdf <- get_richness(
    poly = get_active_feature(input),
    zoom = as.integer(input$map_zoom),
    taxa_selections = taxa_selections$selections()
  )
  maplibre_proxy("map") |>
    add_richness(gdf)
})
# bot selection triggers these via waterfall, not manual

# Ex Show a feature user has drawn on the map
observeEvent(input$get_features, {
  print("Extracting drawn features")

  gdf <- get_drawn_features(maplibre_proxy("map"))
  active_feature(gdf)
  print(gdf)
})

observeEvent(input$current_bbox, {
  gdf <- get_polygon_bbox(input$map_bbox)
  active_feature(gdf)
  # Should we grab polygons from active layer inside bbox instead to cache?
})

# Ex: Get POINT data from geocoder (OSM)
observeEvent(input$map_geocoder$result, {
  gdf <- geocoder_to_gdf(input$map_geocoder)
  # active_feature(gdf)

  # Get value of various layers based on containing hex of given zoom
  print(gdf)
})


# Update the fill color
observeEvent(input$color, {
  maplibre_proxy("map") |>
    set_paint_property(input$layer_selection, "fill-color", input$color)
})

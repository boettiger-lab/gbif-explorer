library(shiny)
library(bslib)
library(mapgl)
library(sf)
library(dplyr)
library(duckdbfs)
library(colourpicker)
library(overture)
source("data-layers.R")


ui <- page_sidebar(
  title = "Interactive feature selection",
  # Add custom CSS for smaller font size

  sidebar = sidebar(
    card(
      card_header("Areas"),
      radioButtons(
        "layer_selection",
        NULL,
        choices = list(
          "Countries" = "country_layer",
          "States" = "region_layer",
          "Counties" = "county_layer",
          "Tracts" = "tract_layer",
          #         "Protected Areas" = "park_layer",
          #         "US fires" = "fire_layer",
          "None" = "none"
        ),
        selected = "country_layer"
      ),
    ),
    card(card_header("filters"), actionLink("clear_filters", "🧹"), ),
    # Move states, countries to PMTiles Overture layers
    # Add support for filters

    hr(),

    card(
      card_header("Biodiversity"),
      actionLink("get_richness", "species richness")
    ),

    accordion(
      accordion_panel(
        "Filter taxa",
      )
    ),

    br(),

    accordion(
      accordion_panel(
        "Controls",
        input_switch("toggle_controls", "map controls", value = TRUE),
        actionButton("get_features", "Get drawing"),
        actionButton("current_bbox", "Get screen"),
        sliderInput(
          "max_zoom",
          "Max Zoom Level",
          min = 1,
          max = 15,
          value = 8,
          step = 1
        ),
        colourInput("color", "Set layer color", value = "blue"),
        open = FALSE
      )
    ),
  ),
  card(
    full_screen = TRUE,
    maplibreOutput("map")
  ),
  includeMarkdown(
    "
  "
  ),
)


server <- function(input, output, session) {
  # Set up the map:
  output$map <- renderMaplibre({
    m <- mapgl::maplibre(
      zoom = 1,
      center = c(-80, 20),
      maxZoom = input$max_zoom
    )

    m <- m |>
      add_pmtiles_source("tract_source", tract) |>
      add_pmtiles_source("county_source", counties) |>
      add_pmtiles_source("region_source", regions) |>
      add_pmtiles_source("country_source", countries)

    m <- m |>
      add_fullscreen_control() |>
      add_globe_control() |>
      add_draw_control() |>
      add_geocoder_control()

    # Add any layer you want to be on by default
    m |> add_countries()
  })

  # PMTiles layer filter
  layer_filter <- reactiveVal(NULL)

  # React to layer selection
  observeEvent(input$layer_selection, {
    proxy <- maplibre_proxy("map")

    # Clear all configured layers
    for (layer_id in names(layer_config)) {
      proxy |> clear_layer(layer_id)
    }

    # Get configuration for selected layer
    config <- layer_config[[input$layer_selection]]

    # Always add the selected layer
    proxy <- proxy |> config$add_layer()

    # Handle filter logic
    if (config$clear_filter) {
      layer_filter(NULL)
    }
    proxy |> set_filter(input$layer_selection, layer_filter())
  })

  # React to selected feature
  observeEvent(input$map_feature_click, {
    # NOTE: This reacts to drawing features too

    # Look up the configuration for the active layer.
    x <- input$map_feature_click
    config <- layer_config[[x$layer]]
    name <- x$properties[[config$name_property]]

    # Filter next layer to the clicked feature
    if (!is.null(config$next_layer) && !is.null(name)) {
      selected <- x$properties[[config$filter_property]]
      print(paste("Clicked:", name))

      # Hack case: only US has below county_layer
      if (x$layer == "county_layer" && x$properties[["country"]] != "US") {
        print("no more layers")
        return()
      }

      # Set the filter to focus on the clicked feature only
      layer_filter(list(
        "==",
        get_column(config$filter_column),
        selected
      ))

      # fly_to, jump_to, or ease_to ?
      maplibre_proxy("map") |>
        fly_to(zoom = input$map_zoom + 2, center = c(x$lng, x$lat))

      # and activate child layer inside it
      updateRadioButtons(
        session,
        "layer_selection",
        selected = config$next_layer
      )

      # Optional logging to console
      print(paste(
        "Activating",
        config$next_layer,
        "and filtering",
        config$filter_column,
        "=",
        selected
      ))
    }
  })

  observeEvent(input$clear_filters, {
    maplibre_proxy("map") |> set_filter(input$layer_selection, NULL)
  })

  # Ex Show a feature user has drawn on the map
  observeEvent(input$get_features, {
    print("Extracting drawn features")

    drawn_features <- get_drawn_features(maplibre_proxy("map"))
    print(drawn_features)
  })

  observeEvent(input$current_bbox, {
    bbox <- sf::st_bbox(unlist(input$map_bbox), crs = 4326)
    print("current bbox:")
    print(bbox)

    # Should we grab polygons from active layer inside bbox?
    # Could allow for better cache behavior
  })

  # Ex: Get POINT data from geocoder (OSM)
  # could then react by operating on hex or some containing polygon
  observeEvent(input$map_geocoder$result, {
    temp <- tempfile(fileext = ".geojson")
    output <- list(
      type = "FeatureCollection",
      features = input$map_geocoder$result$features
    )
    jsonlite::write_json(
      output,
      temp,
      auto_unbox = TRUE
    )
    geo <- sf::st_read(temp)

    # get parent polygon from active layer via duckdbfs st_contains
    print(geo)
  })

  # Toggle draw controls
  observeEvent(input$toggle_controls, {
    proxy <- maplibre_proxy("map") |> clear_controls()
    if (input$toggle_controls) {
      proxy |>
        add_fullscreen_control() |>
        add_globe_control() |>
        add_draw_control() |>
        add_geocoder_control()
    }
  })
  # Update the fill color
  observeEvent(input$color, {
    maplibre_proxy("map") |>
      set_paint_property(input$layer_selection, "fill-color", input$color)
  })
}

shinyApp(ui, server)

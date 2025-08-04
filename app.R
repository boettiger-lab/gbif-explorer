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
      card_header("Layer"),
      radioButtons(
        "layer_selection",
        NULL,
        choices = list(
          "Countries" = "country_layer",
          "States" = "region_layer",
          "Counties" = "county_layer",
          "Tracts" = "tract_layer",
          "Parks" = "park_layer",
          "Fires" = "fire_layer"
        ),
        selected = "country_layer"
      ),
    ),

    # Move states, countries to PMTiles Overture layers
    # Add support for filters

    hr(),
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
  output$map <- renderMaplibre({
    m <- mapgl::maplibre(zoom = 1, maxZoom = input$max_zoom)

    m <- m |>
      add_pmtiles_source(id = "tract_source", url = tract) |>
      add_pmtiles_source(id = "county_source", url = counties) |>
      add_source("region_source", us_states) |>
      add_source("country_source", countries)

    m <- m |>
      add_fullscreen_control() |>
      add_globe_control() |>
      add_draw_control() |>
      add_geocoder_control()

    # Add any layer you want to be on by default
    m |> add_countries()
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

  # PMTiles layer filter
  county_filter <- reactiveVal(NULL)

  # Handle layer selection changes
  observeEvent(input$layer_selection, {
    proxy <- maplibre_proxy("map")

    # Clear all layers first
    proxy |>
      clear_layer("county_layer") |>
      clear_layer("region_layer") |>
      clear_layer("country_layer")

    # Only reset county filter when switching away from counties to other layers
    if (input$layer_selection != "county_layer") {
      county_filter(NULL)
    }

    # Add the selected layer
    if (input$layer_selection == "country_layer") {
      proxy |> add_countries()
    } else if (input$layer_selection == "region_layer") {
      proxy |> add_states()
    } else if (input$layer_selection == "county_layer") {
      proxy <- proxy |> add_counties()
      # Apply filter if one exists
      if (!is.null(county_filter())) {
        proxy |> set_filter("county_layer", county_filter())
      }
    }
  })

  # Ex: Select the feature the user clicked on and zoom into it
  # This reacts to drawing features too
  observeEvent(input$map_feature_click, {
    my_layers <- c("country_layer", "region_layer", "county_layer")
    x <- input$map_feature_click
    print(x$properties)

    if (is.null(x$properties$mode)) {
      if (x$layer == "country_layer") {
        # Handle country click - zoom to country and switch to states
        country_name <- x$properties$iso_a2
        print(paste("Clicked country:", country_name))

        # For US-focused app, if user clicks on USA
        if (country_name == "US") {
          # Zoom to US bounds and switch to states layer
          maplibre_proxy("map") |> fit_bounds(us_states, animate = TRUE)

          # Update radio button to show states
          updateRadioButtons(
            session,
            "layer_selection",
            selected = "region_layer"
          )
        }
      }

      if (x$layer == "region_layer") {
        state_abbr <- x$properties$ST_ABBR
        print(state_abbr)

        county_filter(list("==", get_column("ST_ABBR"), state_abbr))

        state <- us_states |> filter(ST_ABBR == state_abbr)
        maplibre_proxy("map") |> fit_bounds(state)

        # Update radio button to show counties
        updateRadioButtons(
          session,
          "layer_selection",
          selected = "county_layer"
        )
      }

      if (x$layer == "county_layer") {
        county <- x$properties$COUNTY
        print(county)
      }
    }
    # use x$layer and x$properties$FIPS ( ID column) to extract geom and plot
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

  # Ex: Update the fill color
  observeEvent(input$color, {
    maplibre_proxy("map") |>
      set_paint_property(input$layer_selection, "fill-color", input$color)
  })
}

shinyApp(ui, server)

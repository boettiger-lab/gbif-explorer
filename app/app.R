library(conflicted)
conflicts_prefer(dplyr::filter)
conflicts_prefer(mapgl::interpolate)

library(shiny)
library(shinychat)
library(shinybusy)
library(bslib)
library(colourpicker)
library(markdown)

library(sf)
library(dplyr)
library(duckdbfs)

library(mapgl)
library(stringr)
library(jsonlite)
library(glue)
source("data-layers.R")
source("utils.R")
source("taxa-filter.R")

source("llm-gbif.R")

# Required for h3j write
duckdb_secrets()

ui <- page_sidebar(
  title = "Interactive feature selection",
  shinybusy::add_busy_spinner("fading-circle"),

  sidebar = sidebar(
    width = 300,
    card(
      accordion(
        accordion_panel(
          "Area selector",
          p("Click on a highlighted region to select."),
          radioButtons(
            "layer_selection",
            NULL,
            choices = list(
              "Off" = "none",
              "Countries" = "country_layer",
              "States" = "region_layer",
              "Counties" = "county_layer",
              "Tracts" = "tract_layer"
              #         "Protected Areas" = "park_layer",
              #         "US fires" = "fire_layer",
            ),
            selected = "none"
          ),
          p("")
        )
      )
    ),

    card(
      card_header("Biodiversity"),
      chat_ui("chat", placeholder = "hummingbirds"),
      actionLink("clear_richness", "🧹 clear richness")
    ),

    br(),

    accordion(
      accordion_panel(
        "Controls",
        input_switch("hillshade_basemap", "Hillshade Basemap", value = FALSE),
        input_switch("toggle_controls", "map controls", value = TRUE),
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
  taxa_selections <- taxonomicSelectorServer("taxa_selector")

  # Dynamic variables:
  layer_filter <- reactiveVal(NULL) # active layer filter (maplibre filter)
  taxa_filter <- reactiveVal(NULL) # active species filter (list)
  active_feature <- reactiveVal(NULL) # active polygon
  selected_feature <- reactiveVal(NULL)

  # Waterfall strategy to determine feature
  get_active_feature <- function(input) {
    gdf <- active_feature()

    if (is_empty(gdf)) {
      print("No feature selected, checking for drawing")
      gdf <- get_drawn_features(maplibre_proxy("map"))
    }
    if (is_empty(gdf)) {
      print("No drawing found, checking geocoder")
      gdf <- geocoder_to_gdf(input$map_geocoder)
    }
    if (is_empty(gdf)) {
      print("No geocoder, getting current bbox")
      bbox <- input$map_bbox
      print(bbox)
      if (!is.null(bbox)) {
        gdf <- get_polygon_bbox(bbox)
      }
    }

    if (is_empty(gdf)) {
      warning("No selection found, using default!")
      return(spData::us_states)
    }

    gdf
  }

  # Set up the map:
  output$map <- renderMaplibre({
    m <- mapgl::maplibre(
      zoom = 2,
      center = c(-80, 20),
      maxZoom = input$max_zoom
    )

    m <- m |>
      add_pmtiles_source("tract_source", tract) |>
      add_pmtiles_source("county_source", counties) |>
      add_pmtiles_source("region_source", regions) |>
      add_pmtiles_source("country_source", countries) |>
      add_hillshade_source()

    m <- m |>
      add_fullscreen_control() |>
      add_globe_control() |>
      add_draw_control() |>
      add_geocoder_control()

    m <- m |> add_raster_layer(id = "natgeo", source = "natgeo")
    m
  })

  observeEvent(input$hillshade_basemap, {
    if (input$hillshade_basemap) {
      maplibre_proxy("map") |>
        add_hillshade()
    } else {
      maplibre_proxy("map") |> clear_layer("hills")
    }
  })

  # Update map to show selected layer (polygons)
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

  # Observe chat input
  observeEvent(input$chat_user_input, {
    taxa_selected <- txt_to_taxa(input$chat_user_input)
    resp <- paste(
      "selected:\n",
      jsonlite::toJSON(taxa_selected, pretty = TRUE, auto_unbox = TRUE)
    )
    chat_append("chat", resp)
    chat_clear("chat")

    # optionally - store the selection as global variable for future reactions
    taxa_filter(taxa_selected)

    # we can react right away, computing richness and updating map
    gdf <- get_richness(
      poly = get_active_feature(input),
      zoom = as.integer(input$map_zoom),
      taxa_selections = taxa_selected
    )
    maplibre_proxy("map") |>
      add_richness(gdf)
  })

  # Zoom into selected feature, move down a layer, show resulting child features
  observeEvent(input$map_feature_click, {
    # NOTE: This reacts to drawing features too

    # Look up the configuration for the active layer.
    x <- input$map_feature_click
    config <- layer_config[[x$layer]]

    if (is.null(config)) {
      return()
    }

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

      lazy_gdf <- activate_from_config(x$properties$id, config)
      active_feature(lazy_gdf) # can a lazy feature be global var?
    }
  })

  #logging helper
  print_selections <- function() {
    print(paste(
      "Computing biodiversity for",
      digest::digest(active_feature()),
      "at zoom",
      as.integer(input$map_zoom),
      "for taxa:",
      paste(taxa_filter(), collapse = ":")
    ))
  }

  observeEvent(input$get_richness, {
    gdf <- get_richness(
      poly = get_active_feature(input),
      zoom = as.integer(input$map_zoom),
      taxa_selections = taxa_filter()
    )

    maplibre_proxy("map") |>
      add_richness(gdf)
  })

  observeEvent(input$clear_filters, {
    maplibre_proxy("map") |> set_filter(input$layer_selection, NULL)
  })

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

  observeEvent(input$clear_richness, {
    maplibre_proxy("map") |> clear_layer("richness")
  })

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

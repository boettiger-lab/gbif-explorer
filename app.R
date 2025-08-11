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
    #card(card_header("filters"), actionLink("clear_filters", "🧹"), ),
    # Move states, countries to PMTiles Overture layers
    # Add support for filters

    card(
      card_header("Biodiversity"),
      chat_ui("chat", placeholder = "hummingbirds"),
      actionLink("get_richness", "get sp richness"),
      actionLink("clear_richness", "🧹")
    ),

    br(),

    accordion(
      accordion_panel(
        "Taxa selector",
        taxonomicSelectorCard(
          "taxa_selector",
          "Select Taxa",
          include_reset = TRUE
        ),
        open = FALSE
      ),
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
  taxa_selections <- taxonomicSelectorServer("taxa_selector")

  # Dynamic variables:
  layer_filter <- reactiveVal(NULL)
  selected_feature <- reactiveVal(NULL)
  taxa_filter <- reactiveVal(NULL)

  observeEvent(input$chat_user_input, {
    taxa_selected <- txt_to_taxa(input$chat_user_input)
    #print(taxa_selected)
    taxa_filter(taxa_selected)
    chat_append("chat", "done")
  })
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

      # record info about currently selected feature
      selected_feature(list(
        name = name,
        layer = x$layer,
        config = config,
        properties = x$properties
      ))

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

  observeEvent(input$get_richness, {
    # do nothing if no feature is selected?  Alternately grab screen?
    x <- selected_feature()
    if (is.null(x)) {
      warning("select a feature first")
      return()
    }

    # selected_feature() must return these. make this more robust
    id <- x$properties$id
    zoom <- as.integer(input$map_zoom)
    poly <- open_dataset(x$config$parquet)

    # FIXME abstract this into selected_feature() behavior.
    if ("id" %in% colnames(poly)) {
      poly <- poly |>
        filter(.data[["id"]] == !!id)
    }
    if ("geometry" %in% colnames(poly)) {
      poly <- poly |> rename(geom = geometry)
    }

    selected_taxa <- taxa_filter()

    print(selected_taxa)
    print(paste(
      "Computing biodiversity for",
      x$name,
      "at zoom",
      zoom,
      "for taxa:",
      paste(selected_taxa, collapse = ":")
    ))
    gdf <- get_richness(
      poly = poly,
      zoom = zoom,
      taxa_selections = selected_taxa
    )

    print(paste(nrow(gdf), "features"))
    maplibre_proxy("map") |>
      clear_layer(x$config$next_layer) |>
      add_richness(gdf)
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
    gdf <- get_polygon_bbox(input$map_bbox)
    activate_polygon(gdf, name = "gdf")
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

  observeEvent(input$clear_richness, {
    maplibre_proxy("map") |> clear_layer("richness")
  })

  # Manual taxonomic controls
  observeEvent(taxa_selections$filter_trigger(), {
    taxa_filter(taxa_selections$selections())
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

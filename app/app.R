library(conflicted)
conflicts_prefer(dplyr::filter)
conflicts_prefer(mapgl::interpolate)

library(shiny)
library(shinychat)
library(shinybusy)
library(bslib)
library(colourpicker)
library(markdown)
library(memoise)
library(sf)
library(dplyr)
library(duckdbfs)
library(mapgl)
library(stringr)
library(jsonlite)
library(glue)

print(packageVersion("duckdb"))

duckdbfs::load_h3()
duckdbfs::load_spatial()

source("data-layers.R")
source("hex-tools.R")
source("utils.R")
source("llm-gbif.R")
source("tools.R")
source("taxa-filter.R")


MAXZOOM <- 10
duckdb_secrets()
duckdb_config(threads = 100)
ui <- page_sidebar(
  title = "Explore Global Biodiversity",
  includeMarkdown(
    "Activate the area selector to select specific regions (countries, states, counties, etc) by clicking on the map.
Then, enter the species or taxonomic group of interest into the biodiversity search bar.

Smaller areas will be faster to compute!  Zoom in further to show richness with a finer resolution hex grid, but higher resolutions will be much slower and may crash.

*Experimental*: use the draw tools to create any custom region.  Or simply zoom in to the desired region with scrolling and activate richness for the entire visible area.

"
  ),
  shinybusy::add_busy_spinner("fading-circle"),
  sidebar = sidebar(
    width = 300,
    card(
      accordion(
        accordion_panel(
          "Area selector",
          radioButtons(
            "layer_selection",
            NULL,
            choices = list(
              "Off" = "none",
              "Countries" = "country_layer",
              "States" = "region_layer",
              "Counties" = "county_layer",
              "Tracts" = "tract_layer",
              "Protected Areas" = "pad_layer"
              #         "US fires" = "fire_layer",
            ),
            selected = "country_layer"
          ),
          actionLink("clear_area", "🧹 clear selection"),
          p("")
        )
      )
    ),
    fluidRow(
      # Taxonomic selector card
      taxonomicSelectorCard(
        "taxa_selector",
        "Select Taxa"
      )
    ),
    card(
      card_header("Biodiversity"),
      # chat_ui("chat", placeholder = "hummingbirds"),
      actionLink("get_richness", "🐦 GBIF species richness"),
      actionLink("get_carbon", "🌱 vulnerable carbon"),
    ),
    card(
      card_header("Resolution"),
      sliderInput(
        "resolution",
        NULL,
        min = 1,
        max = MAXZOOM,
        value = 2,
        step = 1
      ),
      input_switch("show_hexes", "show hexes", value = FALSE)
    ),
    br(),
    input_switch("toggle_natgeo", "natgeo", value = TRUE),
    input_switch("hillshade_basemap", "hillshade", value = FALSE),
    input_switch("toggle_controls", "map controls", value = TRUE),
    radioButtons(
      "basemap",
      "Basemap:",
      choices = list(
        "light" = carto_style("voyager"),
        "dark" = carto_style("dark-matter"),
        "satellite" = maptiler_style("satellite")
      ),
      selected = "natgeo"
    )
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
  # Dynamic variables:
  layer_filter <- reactiveVal(NULL) # active layer filter (maplibre filter)
  taxa_filter <- reactiveVal(NULL) # active species filter (list)
  active_feature <- reactiveVal(NULL) # active polygon

  taxa_selections <- taxonomicSelectorServer("taxa_selector")

  # Update taxa selector tool to drill down on taxonomic selections
  observe({
    selections <- taxa_selections$selections()
    print(paste(
      "Taxa selections updated:",
      paste(names(selections), selections, sep = "=", collapse = ", ")
    ))
    taxa_filter(selections)
  })

  # Waterfall strategy to determine feature selection:
  # FIXME select all (visible) protected areas? select protected areas by filtering them?

  # Set up the map:
  output$map <- renderMaplibre({
    m <- mapgl::maplibre(
      zoom = 2,
      center = c(-100, 30),
      maxZoom = MAXZOOM
    )

    m <- m |>
      add_pmtiles_source("tract_source", tract, promoteId = "primary") |>
      add_pmtiles_source("county_source", counties, promoteId = "primary") |>
      add_pmtiles_source("region_source", regions, promoteId = "primary") |>
      add_pmtiles_source("country_source", countries, promoteId = "primary") |>
      add_pmtiles_source("pad_source", pad_us_4, promoteId = "Unit_Nm") |>
      add_hillshade_source()

    m <- m |>
      add_fullscreen_control() |>
      add_globe_control() |>
      add_draw_control() |>
      add_geocoder_control()

    m <- m |>
      add_raster_layer(id = "natgeo_layer", source = "natgeo_source") |>
      set_layout_property("natgeo_layer", "visibility", "none")

    m <- m |> add_hillshade(visibility = "none")

    m <- m |> richness_layer() # default richness layer
    m <- m |> carbon_layer() # default carbon layer

    m |> add_countries()
  })

  # Update map to show selected layer (polygons)
  observeEvent(input$layer_selection, {
    proxy <- maplibre_proxy("map")

    if (input$layer_selection == "none") {
      active_feature(NULL)
      layer_filter(NULL)
    }

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

    ## HACK: use an initial layer filter for PAD_US:
    if (input$layer_selection == "pad_layer" && is.null(layer_filter())) {
      pad_gap12_filter <- list(
        "in",
        list("get", "GAP_Sts"),
        list("literal", c("1", "2"))
      )

      layer_filter(pad_gap12_filter)
    }

    ## FIXME layer_filter should know what layer it applies to!
    proxy |> mapgl::set_filter(input$layer_selection, layer_filter())
  })

  # Zoom into selected feature, move down a layer, show resulting child features
  observeEvent(input$map_feature_click, {
    x <- input$map_feature_click
    config <- layer_config[[x$layer]]
    if (is.null(config)) {
      return()
    }
    name <- x$properties[[config$name_property]]

    # Filter next layer to the clicked feature
    if (!is.null(config$next_layer) && !is.null(name)) {
      selected <- x$properties[[config$filter_property]]
      id <- x$properties[[config$id_property]]
      print(paste("Clicked:", name, "id:", id, "selected:", selected))

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

      # If we are not changing layers, we can apply the filter now:
      # FIXME this is a bit hacky.  The above does not actually require a next_layer.
      if (config$next_layer == input$layer_selection) {
        maplibre_proxy("map") |>
          set_filter(input$layer_selection, layer_filter())
      }

      # fly_to, jump_to, or ease_to ?
      maplibre_proxy("map") |>
        fly_to(zoom = input$map_zoom + 1, center = c(x$lng, x$lat))

      # and activate child layer inside it
      updateRadioButtons(
        session,
        "layer_selection",
        selected = config$next_layer
      )

      # Set this as the active selection
      lazy_gdf <- activate_from_config(id, config)
      active_feature(lazy_gdf) # can a lazy feature be global var?
    }
  })

  observeEvent(input$get_richness, {
    poly <- get_active_feature(active_feature(), input)

    if (input$show_hexes) {
      gdf <- get_richness(
        poly = poly,
        zoom = as.integer(input$resolution),
        taxa_selections = taxa_filter()
      )
    } else {
      layer <- layer_config[[input$layer_selection]]$parent_layer
      child_poly <- child_polygons(poly, layer, layer_config)
      gdf <- get_zonal_richness(
        child_poly,
        zoom = as.integer(input$resolution),
        taxa_selections = taxa_filter()
      )
    }

    print(gdf)
    maplibre_proxy("map") |>
      set_source("richness", gdf)
  })

  observeEvent(input$get_carbon, {
    poly <- get_active_feature(active_feature(), input)

    if (input$show_hexes) {
      gdf <- get_carbon(
        poly = poly,
        zoom = as.integer(input$resolution)
      )
    } else {
      layer <- layer_config[[input$layer_selection]]$parent_layer
      child_poly <- child_polygons(poly, layer, layer_config)
      gdf <- get_mean_carbon(child_poly, zoom = as.integer(input$resolution))
    }

    print(gdf)
    maplibre_proxy("map") |>
      set_source("carbon", gdf)
  })

  # Layer selection tools
  observeEvent(input$clear_filters, {
    maplibre_proxy("map") |> set_filter(input$layer_selection, NULL)
  })

  observeEvent(input$clear_area, {
    active_feature(NULL)
    layer_filter(NULL)
    updateRadioButtons(
      session,
      "layer_selection",
      selected = "none"
    )
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
  # is it better to set visibility or add/clear layer?
  observeEvent(input$hillshade_basemap, {
    if (input$hillshade_basemap) {
      maplibre_proxy("map") |>
        set_layout_property("hills", "visibility", "visible")
      # add_hillshade()
    } else {
      maplibre_proxy("map") |>
        set_layout_property("hills", "visibility", "none")
      # clear_layer("hills")
    }
  })
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

  # Update resolution slider to match map zoom
  observeEvent(input$map_zoom, {
    updateSliderInput(session, "resolution", value = input$map_zoom)
  })

  observeEvent(input$basemap, {
    # doesn't toggle here but works in debug, hmm
    if (input$basemap == "natgeo") {
      maplibre_proxy("map") |>
        set_layout_property("natgeo_layer", "visibility", "visible")
    } else {
      maplibre_proxy("map") |>
        set_layout_property("natgeo_layer", "visibility", "none") |>
        set_style(input$basemap)
    }
  })

  # Observe chat input
  observeEvent(input$chat_user_input, {
    taxa_selected <- txt_to_taxa(input$chat_user_input)

    resp <- bot_response(taxa_selected, input$resolution)
    print(resp)
    chat_append("chat", resp)

    # optionally - store the selection as global variable for future reactions
    taxa_filter(taxa_selected)

    # we can react right away, computing richness and updating map
    gdf <- get_richness(
      poly = get_active_feature(active_feature(), input),
      zoom = as.integer(input$resolution),
      taxa_selections = taxa_selected
    )

    chat_clear("chat")

    maplibre_proxy("map") |>
      set_source("richness", gdf)
  })
}

shinyApp(ui, server)

get_polygon_bbox <- function(bbox) {
  # safe return
  bbox$xmin <- max(bbox$xmin, -178)
  bbox$xmax <- min(bbox$xmax, 178)
  bbox$ymin <- max(bbox$ymin, -89.99)
  bbox$ymax <- min(bbox$ymax, 89.99)

  bbox <- sf::st_bbox(unlist(bbox), crs = 4326)
  gdf <- st_sf(geometry = st_as_sfc(st_bbox(bbox)))
  gdf
}

# Returns results of geocoder as gdf (sf object) with POINT geometry
geocoder_to_gdf <- function(map_geocoder) {
  print(map_geocoder)
  if (is.null(map_geocoder)) {
    return(NULL)
  }
  temp <- tempfile(fileext = ".geojson")
  output <- list(
    type = "FeatureCollection",
    features = map_geocoder$result$features
  )
  jsonlite::write_json(
    output,
    temp,
    auto_unbox = TRUE
  )
  gdf <- sf::st_read(temp)
  gdf
}

# is it okay for this to be lazy?
activate_from_config <- function(id, config) {
  poly <- duckdbfs::open_dataset(config$parquet)
  id_col <- config$id_property
  # FIXME abstract this into selected_feature() behavior.
  if (id_col %in% colnames(poly)) {
    poly <- poly |>
      dplyr::filter(.data[[id_col]] == !!id)
  }

  return(poly)
}

is_empty <- function(df) {
  if (is.null(df)) {
    return(TRUE)
  }
  if (inherits(df, "tbl_lazy")) {
    df <- df |>
      head(1) |>
      dplyr::collect()
  }
  if (nrow(df) < 1) {
    return(TRUE)
  }

  FALSE
}


bot_response <- function(taxa_selected, zoom) {
  if (length(taxa_selected) < 1) {
    taxa_msg <- "all species"
  } else {
    taxa_msg <- paste0(taxa_selected, collapse = ":")
  }

  resp <- paste(
    "Counting unique occurrences at zoom",
    as.integer(zoom),
    "for\n",
    taxa_msg
  )
  resp
}


# use layer-config to compute child polygons (from geoparquet files)
# given active duckdb connection to polygon subset, a layer, and layer config
child_polygons <- function(poly, layer, layer_config) {
  parent <- layer_config[[layer]]
  children <- layer_config[[parent$next_layer]]

  filter_property <- poly |> pull(all_of(parent$filter_property))
  filter_column <- parent$filter_column
  child_poly <-
    open_dataset(children$parquet) |>
    filter(.data[[filter_column]] %in% filter_property)

  child_poly
}

get_active_feature <- function(gdf, input) {
  if (is_empty(gdf)) {
    print("No feature selected, checking for drawing")
    gdf <- mapgl::get_drawn_features(mapgl::maplibre_proxy("map"))
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

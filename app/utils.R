sf_to_lazy <- function(gdf) {
  if (inherits(gdf, "sf")) {
    tmp = file.path(tempdir(), "current_drawing.fgb")
    unlink(tmp) # i.e. overwrite
    # do we want to write with sf? as geojson?
    sf::st_write(gdf, tmp)
    gdf <- duckdbfs::open_dataset(tmp)
  }
  gdf
}

# FIXME Debug testing:
# Can get_h3_aoi return no hexes, e.g. point geom,

get_h3_aoi <- function(aoi, precision = 6L) {
  aoi <- sf_to_lazy(aoi) # can't memoise lazy

  get_h3_aoi_(aoi, precision)
}


# Do we ever need disk cache for this?
get_h3_aoi_ <- memoise::memoise(function(
  aoi,
  precision = 6L,
  h3_column = NULL
) {
  # if aoi is in-memory, move to duckdb first

  index <- as.integer(0L) # index for h0-partitioned data
  duckdbfs::load_h3()

  # consider auto-retry at higher precision if subset is empty.
  precision <- as.integer(precision)

  # Column name will be based on resolution
  if (is.null(h3_column)) {
    h3_column <- paste0("h", precision)
  }

  # assumes geom column is "geom"
  if ("geometry" %in% colnames(aoi)) {
    aoi <- aoi |> rename(geom = geometry)
  }

  # CHECK IF POINT GEOM, JUST RETURN hex of desired precision at point!!
  # Do not call h3_polygon_wkt...

  # multipolygon dump may not be needed for draw tools.
  h3_aoi <- aoi |>
    # dump multi-polygons to polygons
    mutate(
      poly = array_extract(unnest(st_dump(geom)), "geom"),
      # compute h3 cells of each polygon
      h3id = h3_polygon_wkt_to_cells(poly, {
        precision
      }),
      # unnest: one h3 per row
      h3id = unnest(h3id)
    ) |>
    # Also tell me the h0.
    mutate(
      h0 = h3_h3_to_string(h3_cell_to_parent(h3id, {
        index
      })),
      h3id = h3_h3_to_string(h3id)
    ) |>
    mutate(h0 = toupper(h0), h3id = toupper(h3id)) |>
    select(h0, h3id) |>
    rename(!!h3_column := h3id)

  h3_aoi |> as_view("h3_aoi")

  h3_aoi
})


open_gbif_partition <- function(
  subset,
  server = Sys.getenv("AWS_PUBLIC_ENDPOINT", Sys.getenv("AWS_S3_ENDPOINT"))
) {
  if (length(subset) < 1) {
    return(open_dataset(glue("s3://public-gbif/hex/"), tblname = "gbif"))
  }

  urls <- paste0(
    glue("https://{server}/public-gbif/hex/h0="),
    subset,
    "/part0.parquet"
  )
  gbif <- open_dataset(urls, tblname = "gbif")
}

open_gbif_region <- function(
  poly_hexed,
  server = Sys.getenv("AWS_PUBLIC_ENDPOINT", Sys.getenv("AWS_S3_ENDPOINT"))
) {
  subset <- poly_hexed |> distinct(h0) |> pull()
  gbif <- open_gbif_partition(subset, server)

  return(gbif)
}

filter_gbif_taxa <- function(gbif, selections) {
  # If no selections made, return original dataset
  if (length(selections) == 0) {
    return(gbif)
  }

  # Start with the original dataset
  filtered_gbif <- gbif

  # Apply filters for each selected taxonomic rank
  for (rank in names(selections)) {
    if (rank %in% colnames(gbif)) {
      filtered_gbif <- filtered_gbif |>
        dplyr::filter(.data[[rank]] == !!selections[[rank]])
    }
  }

  return(filtered_gbif)
}


is_cached <- function(s3, recursive = FALSE) {
  tryCatch(
    {
      df <- open_dataset(s3, recursive = recursive)
      inherits(df, "tbl_sql")
    },
    error = function(e) FALSE,
    finally = FALSE
  )
}

## Memoise the slow stuff
get_richness_ <- function(
  poly_hexed,
  zoom,
  taxa_selections = list(),
  server
) {
  gbif <- open_gbif_region(poly_hexed, server)
  gbif <- filter_gbif_taxa(gbif, taxa_selections)

  # zoom is already determined by poly_hexed
  hexcols <- poly_hexed |> colnames()
  index <- hexcols[2]

  hash <- digest::digest(list(poly_hexed, taxa_selections, zoom))
  richness_cache <- paste0(
    "s3://public-data/gbif-cache/richness/",
    hash,
    fileext = ".parquet"
  )

  if (!is_cached(richness_cache)) {
    gbif |>
      select(taxonkey, !!index) |>
      inner_join(poly_hexed) |>
      distinct() |>
      count(!!index) |>
      mutate(logn = log(n), value = logn / max(logn)) |>
      mutate(geom = h3_cell_to_boundary_wkt(!!index)) |>
      write_dataset(richness_cache)
  }

  richness_cache
}

get_richness <- function(
  poly,
  zoom,
  taxa_selections = list(),
  max_features = getOption("shiny_max_features", 20000L),
  warning = TRUE,
  server = Sys.getenv("AWS_PUBLIC_ENDPOINT", Sys.getenv("AWS_S3_ENDPOINT"))
) {
  ## This step is memoised (after materializing the poly)
  poly_hexed <- get_h3_aoi(poly, as.integer(zoom))

  richness_cache <- get_richness_(
    poly_hexed,
    zoom,
    taxa_selections,
    server
  )

  gbif <- open_dataset(richness_cache, recursive = FALSE)

  if (warning) {
    n_features <- gbif |> count() |> pull(n)
    if (n_features > max_features) {
      warning(paste("returning only first", max_features, "of", n_features))
    }
  }

  gbif <- gbif |>
    head(max_features) |> # max number of features
    collect() |>
    st_as_sf(wkt = "geom", crs = 4326)

  gbif
}


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

  # FIXME abstract this into selected_feature() behavior.
  if ("id" %in% colnames(poly)) {
    poly <- poly |>
      dplyr::filter(.data[["id"]] == !!id)
  }

  return(poly)
}

is_empty <- function(df) {
  if (is.null(df)) {
    return(TRUE)
  }
  if (inherits(df, "tbl_lazy")) {
    df <- df |> head(1) |> dplyr::collect()
  }
  if (nrow(df) < 1) {
    return(TRUE)
  }

  FALSE
}

# cache this somehow?  write to digest of gdf instead of fixed
sf_to_lazy <- function(gdf) {
  if (inherits(gdf, "sf")) {
    hash <- digest::digest(gdf)
    tmp <- file.path(tempdir(), paste0(hash, ".fgb"))
    if (!file.exists(tmp)) {
      sf::st_write(gdf, tmp, quiet = FALSE)
    }
    gdf <- duckdbfs::open_dataset(tmp)
  } else {
    # materialize it enough to get hash
    hash <- gdf |> duckdbfs::to_sf() |> digest::digest()
  }
  list(gdf = gdf, hash = hash)
}

# FIXME Debug testing:
# Can get_h3_aoi return no hexes, e.g. point geom,
get_h3_aoi <- function(
  aoi,
  precision = 6L,
  h3_column = NULL,
  keep_cols = NULL,
  uppercase = TRUE,
  cache_path = "s3://public-data/gbif-cache/aoi/"
) {
  ## IF aoi is lazy already, we can get_h3_aoi without serializing to fgb.
  ## But we want to materialize it just to get the object hash
  x <- sf_to_lazy(aoi)
  hash <- digest::digest(list(
    x$hash,
    precision,
    h3_column,
    keep_cols,
    uppercase
  ))

  cache <- paste0(
    cache_path,
    hash,
    fileext = ".parquet"
  )

  if (!is_cached(cache)) {
    get_h3_aoi_(x$gdf, precision, h3_column, keep_cols, uppercase) |>
      duckdbfs::write_dataset(cache)
  }
  duckdbfs::open_dataset(cache, recursive = FALSE)
}

get_h3_aoi_ <- function(
  aoi,
  precision = 6L,
  h3_column = NULL,
  keep_cols = NULL,
  uppercase = TRUE
) {
  # can't compute digest if aoi is lazy

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
    aoi <- aoi |> dplyr::rename(geom = geometry)
  }

  # CHECK IF POINT GEOM, JUST RETURN hex of desired precision at point!!
  # Do not call h3_polygon_wkt...

  # multipolygon dump may not be needed for draw tools.
  h3_aoi <- aoi |>
    # dump multi-polygons to polygons
    dplyr::mutate(
      poly = array_extract(unnest(st_dump(geom)), "geom"),
      # compute h3 cells of each polygon
      h3id = h3_polygon_wkt_to_cells(poly, {
        precision
      }),
      # unnest: one h3 per row
      h3id = unnest(h3id)
    ) |>
    # Also tell me the h0.
    dplyr::mutate(
      h0 = h3_h3_to_string(h3_cell_to_parent(h3id, {
        index
      })),
      h3id = h3_h3_to_string(h3id)
    )

  if (uppercase) {
    h3_aoi <- h3_aoi |>
      dplyr::mutate(h0 = toupper(h0), h3id = toupper(h3id))
  }

  h3_aoi <- h3_aoi |>
    dplyr::select(dplyr::any_of(c("h0", "h3id", keep_cols))) |>
    dplyr::rename(!!h3_column := h3id)

  # h3_aoi |> as_view("h3_aoi")

  h3_aoi
}


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

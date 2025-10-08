is_cached <- function(
  s3,
  recursive = FALSE,
  verbose = getOption("verbose", TRUE)
) {
  has_cache <- tryCatch(
    {
      duckdbfs::load_spatial()
      df <- open_dataset(s3, recursive = recursive)
      inherits(df, "tbl_sql")
    },
    error = function(e) FALSE,
    finally = FALSE
  )
  if (verbose) {
    message(paste("cache:", has_cache, s3))
  }
  has_cache
}


# Given an sf object (in RAM), this moves to disk and opens it with duckdb read_geo.
# Object is hashed in RAM
# FIXME Consider moving this to duckdbfs & improving performance, this is stupid

# digest is not robust to the roundtripping here
sf_to_lazy <- function(gdf) {
  if (inherits(gdf, "sf")) {
    hash <- digest::digest(gdf)
    tmp <- file.path(tempdir(), paste0(hash, ".geojson"))
    if (!file.exists(tmp)) {
      sf::st_write(gdf, tmp, quiet = FALSE)
    }
    duckdbfs::load_spatial()
    gdf <- duckdbfs::open_dataset(tmp, format = "sf")
  } else {
    # materialize it enough to get hash
    hash <- gdf |>
      duckdbfs::to_sf() |>
      digest::digest()
  }
  list(gdf = gdf, hash = hash)
}


## Heavyweight bottleneck!
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
  duckdbfs::load_spatial()

  ## the result of this cannot be cached, this is a LAZY connection
  #duckdbfs::open_dataset(cache, recursive = FALSE)

  cache
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

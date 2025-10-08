open_carbon_partition <- function(
  subset,
  server = Sys.getenv("AWS_S3_ENDPOINT", "minio.carlboettiger.info")
) {
  # Fallback case, opens all partitions
  if (length(subset) < 1) {
    return(open_dataset(
      glue("s3://public-carbon/hex/vulnerable-carbon"),
      tblname = "carbon"
    ))
  }
  # open directly as one or more URLs
  urls <- paste0(
    glue("https://{server}/public-carbon/hex/vulnerable-carbon/h0="),
    tolower(subset),
    "/data_0.parquet"
  )
  carbon <- duckdbfs::open_dataset(urls, tblname = "carbon")
}


open_carbon_region <- function(
  poly_hexed,
  server = Sys.getenv("AWS_S3_ENDPOINT", "minio.carlboettiger.info")
) {
  subset <- poly_hexed |>
    dplyr::distinct(h0) |>
    dplyr::pull()

  hexcols <- poly_hexed |> colnames()
  index <- hexcols[2]

  open_carbon_partition(subset, server) |>
    dplyr::select(-h0) |>
    dplyr::mutate(!!index := toupper(!!sym(index))) |>
    dplyr::inner_join(poly_hexed) |>
    dplyr::rename(h3id = !!index)
}

get_carbon <- function(
  poly,
  zoom = 8L,
  id_column = "id",
  max_features = getOption("shiny_max_features", 20000L),
  warning = TRUE,
  verbose = TRUE,
  server = Sys.getenv("AWS_S3_ENDPOINT", "minio.carlboettiger.info"),
  local = FALSE
) {
  duckdbfs::load_h3()

  # get_h3_aoi is self-caching, shared across metrics
  poly_hexed_url <- get_h3_aoi(poly, precision = zoom, keep_cols = id_column)
  poly_hexed <- duckdbfs::open_dataset(poly_hexed_url, recursive = FALSE)

  ## This operation is maybe always fast enough not to cache?
  carbon <- open_carbon_region(poly_hexed, server) |>
    dplyr::group_by(h3id) |>
    dplyr::summarise(carbon = mean(carbon))

  # in-memory gdf will crash above a certain number of hexes
  if (warning) {
    n_features <- carbon |> count() |> pull(n)
    print(paste("computed", n_features, "hexes"))
    if (n_features > max_features) {
      warning(paste("returning only first", max_features, "of", n_features))
    }
  }

  carbon <- carbon |>
    head(max_features) |> # max number of features
    dplyr::mutate(geom = h3_cell_to_boundary_wkt(h3id))

  if (local) {
    carbon <- carbon |>
      dplyr::collect() |>
      sf::st_as_sf(wkt = "geom", crs = 4326)
  } else {
    hash <- digest::digest(list(poly, zoom, id_column, "carbon"))
    s3 <- glue::glue("s3://public-data/cache/gbif-app/carbon/{hash}.geojson")
    carbon |>
      mutate(geom = ST_GeomFromText(geom)) |>
      duckdbfs::to_geojson(s3)
    carbon <- gsub("s3://", glue::glue("https://{server}/"), s3)

  }

  carbon
}


get_mean_carbon <- function() {}

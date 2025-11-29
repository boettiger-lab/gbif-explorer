source("utils.R")

open_carbon_partition <- function(
  subset,
  server = Sys.getenv("AWS_S3_ENDPOINT", "minio.carlboettiger.info"),
  protocol = http_protocol()
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
    glue("{protocol}://{server}/public-carbon/hex/vulnerable-carbon/h0="),
    tolower(subset),
    "/data_0.parquet"
  )
  carbon <- duckdbfs::open_dataset(urls, tblname = "carbon")
}


open_carbon_region <- function(
  poly_hexed,
  server = Sys.getenv("AWS_S3_ENDPOINT", "minio.carlboettiger.info"),
  protocol = http_protocol()
) {
  subset <- poly_hexed |>
    dplyr::distinct(h0) |>
    dplyr::pull()

  hexcols <- poly_hexed |> colnames()
  index <- hexcols[2]

  open_carbon_partition(subset, server, protocol) |>
    dplyr::select(-h0) |>
    dplyr::mutate(!!index := toupper(!!sym(index))) |>
    dplyr::inner_join(poly_hexed) |>
    dplyr::rename(h3id = !!index)
}

get_carbon <- function(
  poly,
  zoom = 8L,
  id_column = "id",
  warning = TRUE,
  verbose = TRUE,
  server = Sys.getenv("AWS_S3_ENDPOINT", "minio.carlboettiger.info"),
  protocol = http_protocol(),
  bucket = "public-data/cache/gbif-app"
) {
  duckdbfs::load_h3()

  # get_h3_aoi is self-caching, shared across metrics
  poly_hexed_url <- get_h3_aoi(poly, precision = zoom, keep_cols = id_column)
  poly_hexed <- duckdbfs::open_dataset(poly_hexed_url, recursive = FALSE)

  ## This operation is maybe always fast enough not to cache?
  carbon <- open_carbon_region(poly_hexed, server, protocol) |>
    dplyr::group_by(h3id) |>
    dplyr::summarise(carbon = mean(carbon)) |>
    dplyr::mutate(value = carbon / max(carbon)) # normalize for color scale

  carbon <- carbon |>
    dplyr::mutate(geom = ST_GeomFromText(h3_cell_to_boundary_wkt(h3id)))

  ## this part should be separate? Or be included in cache logic.
  label <- "carbon"
  hash <- digest::digest(list(carbon, zoom, id_column, label))
  s3 <- glue::glue("s3://{bucket}/{label}/{hash}.geojson")
  duckdbfs::to_geojson(carbon, s3, as_http = TRUE)
}


get_mean_carbon <- function(
  poly,
  zoom = 8L,
  id_column = "id",
  warning = TRUE,
  verbose = TRUE,
  server = Sys.getenv("AWS_S3_ENDPOINT", "minio.carlboettiger.info"),
  protocol = http_protocol(),
  bucket = "public-data/cache/gbif-app"
) {
  # get_h3_aoi is self-caching, shared across metrics
  poly_hexed_url <- get_h3_aoi(poly, precision = zoom, keep_cols = id_column)
  poly_hexed <- duckdbfs::open_dataset(poly_hexed_url, recursive = FALSE)

  ## This operation is maybe always fast enough not to cache?
  carbon <- open_carbon_region(poly_hexed, server, protocol) |>
    dplyr::group_by(.data[[id_column]]) |>
    dplyr::summarise(carbon = mean(carbon)) |>
    dplyr::mutate(value = carbon / max(carbon)) # normalize for color scale

  gdf <- poly |>
    dplyr::select(dplyr::all_of(id_column), geometry) |>
    dplyr::inner_join(carbon, by = id_column) |>
    rename(geom = "geometry")

  label <- "carbon"
  hash <- digest::digest(list(gdf, zoom, id_column, label))
  s3 <- glue::glue("s3://{bucket}/{label}/{hash}.geojson")
  duckdbfs::to_geojson(gdf, s3, as_http = TRUE)
}

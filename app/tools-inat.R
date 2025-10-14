# should this also support richness or single species?
inat_rangemap <- function(
  poly,
  zoom,
  id_column = "id",
  taxa_selections = list(),
  server = Sys.getenv("AWS_S3_ENDPOINT", "minio.carlboettiger.info"),
  bucket = "public-data/cache/gbif-app"
) {
  poly_hexed <- get_h3_aoi(poly, precision = zoom, keep_cols = id_column)
}


get_inat_hexes <- function(
  poly,
  zoom = 4L,
  id_column = "id",
  taxa_selections = list(),
  server = Sys.getenv("AWS_S3_ENDPOINT", "minio.carlboettiger.info"),
  bucket = "public-data/cache/gbif-app"
) {
  duckdbfs::load_h3()

  # get_h3_aoi is self-caching, shared across metrics
  poly_hexed_url <- get_h3_aoi(poly, precision = zoom, keep_cols = id_column)
  poly_hexed <- duckdbfs::open_dataset(poly_hexed_url, recursive = FALSE)

  inat <-
    open_dataset("s3://public-inat/hex") |>
    filter_gbif_taxa(taxa_selections)

  # handle alternate resolutions on the fly?  Or precompute these?
  if (zoom < 4) {
    inat <- inat |> mutate(h3id = unnest(h3_cell_to_children(h4, zoom)))
  } else if (zoom > 4) {
    inat <- inat |>
      mutate(h3id = h3_cell_to_parent(h4, zoom)) |>
      select(-h4) |>
      distinct() # is it worthwhile dropping duplicate rows?
  } else {
    inat <- inat |> rename(h3id = h4)
  }

  inat <- inat |>
    dplyr::inner_join(poly_hexed) |>
    dplyr::count(h3id) |>
    dplyr::mutate(logn = log(n), value = logn / max(logn)) |>
    dplyr::mutate(
      geom = ST_GeomFromText(
        h3_cell_to_boundary_wkt(h3id)
      )
    )

  ## this part should be separate? Or be included in cache logic.
  label <- "inat"
  hash <- digest::digest(list(inat, zoom, id_column, label))
  s3 <- glue::glue("s3://{bucket}/{label}/{hash}.geojson")
  duckdbfs::to_geojson(inat, s3, as_http = TRUE)
}

get_inat_zonal <- function(
  poly,
  zoom = 4L,
  id_column = "id",
  taxa_selections = list(),
  server = Sys.getenv("AWS_S3_ENDPOINT", "minio.carlboettiger.info"),
  bucket = "public-data/cache/gbif-app"
) {
  duckdbfs::load_h3()

  # get_h3_aoi is self-caching, shared across metrics
  poly_hexed_url <- get_h3_aoi(
    poly,
    precision = zoom,
    keep_cols = id_column,
    h3_column = "h3id"
  )
  poly_hexed <-
    duckdbfs::open_dataset(poly_hexed_url, recursive = FALSE) |>
    dplyr::mutate(h3id = tolower(h3id))

  inat <-
    open_dataset("s3://public-inat/hex") |>
    filter_inat_taxa(taxa_selections)

  print("POSITION 1")

  # handle alternate resolutions on the fly?  Or precompute these?
  if (zoom < 4) {
    inat <- inat |> mutate(h3id = unnest(h3_cell_to_children(h4, zoom)))
  } else if (zoom > 4) {
    inat <- inat |>
      mutate(h3id = h3_cell_to_parent(h4, zoom)) |>
      select(-h4) |>
      distinct() # is it worth dropping duplicates?
  } else {
    inat <- inat |> rename(h3id = h4)
  }

  print("POSITION 2")

  print(inat)
  print(poly_hexed)

  inat <- inat |>
    dplyr::inner_join(poly_hexed) |>
    dplyr::count(.data[[id_column]]) |>
    dplyr::mutate(logn = log(n), value = logn / max(logn))

  # join back to poly with geoms
  poly <- poly |>
    dplyr::select(dplyr::all_of(id_column), geometry) |>
    dplyr::inner_join(inat, by = id_column) |>
    rename(geom = "geometry")

  ## this part should be separate? Or be included in cache logic.
  label <- "inat"
  hash <- digest::digest(list(poly, zoom, id_column, label))
  s3 <- glue::glue("s3://{bucket}/{label}/{hash}.geojson")
  duckdbfs::to_geojson(poly, s3, as_http = TRUE)
}

# FIXME do the filter!
filter_inat_taxa <- function(df, taxa_list) {
  df
}

source("utils.R")

open_inat_partition <- function(subset, server, protocol = http_protocol()) {
  if (length(subset) < 1) {
    return(duckdbfs::open_dataset(
      glue::glue("s3://public-inat/range-maps/hex/"),
      tblname = "inat"
    ))
  }

  # could use s3 patterns here still instead of https
  urls <- paste0(
    glue::glue("s3://public-inat/range-maps/hex/h0="),
    subset,
    "/*.parquet"
  )
  open_dataset(urls, tblname = "inat", recursive = FALSE)
}


# open only the partitioned tiles, and then filter down to the polygon
open_inat_region <- function(poly_hexed, server, protocol = http_protocol()) {
  poly_hexed |>
    dplyr::distinct(h0) |>
    dplyr::pull() |>
    tolower() |>
    open_inat_partition(server, protocol)
}


hex_join <- function(dat, aoi) {
  aoi_res <- hex_cols(aoi)
  data_res <- hex_cols(dat)
  aoi <- aoi |> select(-any_of(c("h0", "h3id")))

  hz <- sort(aoi_res, TRUE)[1] # Target zoom of the AOI

  zoom <- as.integer(gsub("^h", "", hz))

  if (any(grepl(hz, data_res))) {
    message(paste("joining area of interest at matched resolution", hz))
    out <- inner_join(aoi, dat, hz) |>
      rename(h3id := !!hz)
  } else {
    # Coarsen the AOI to the data resolution and then filtering join
    smallest <- sort(data_res, TRUE)[1]
    new_zoom <- as.integer(gsub("^h", "", smallest))

    message(paste(
      "joining area of interest smallest resolution of data,",
      smallest
    ))
    out <-
      aoi |>
      mutate(!!smallest := h3_cell_to_parent(.data[[hz]], new_zoom)) |>
      inner_join(dat, by = smallest) |>
      rename(h3id := !!hz)
  }

  out
}


# Adjust this.  Cache the hex join pre species-filter?
open_inat_area <- function(
  poly,
  zoom = 4L,
  id_column = "id",
  taxa_selections = list(),
  server = Sys.getenv("AWS_S3_ENDPOINT", "minio.carlboettiger.info"),
  protocol = http_protocol()
) {
  duckdbfs::load_h3()

  # get_h3_aoi is self-caching, shared across metrics
  poly_hexed_url <- get_h3_aoi(
    poly,
    precision = zoom,
    keep_cols = id_column,
    uppercase = FALSE
  )
  poly_hexed <- duckdbfs::open_dataset(poly_hexed_url, recursive = FALSE)

  inat <- open_inat_region(poly_hexed, server, protocol) |>
    hex_join(poly_hexed) |>
    filter_inat_taxa(taxa_selections)

  inat
}


get_inat_hexes <- function(
  poly,
  zoom = 4L,
  id_column = "id",
  taxa_selections = list(),
  server = Sys.getenv("AWS_S3_ENDPOINT", "minio.carlboettiger.info"),
  protocol = http_protocol(),
  bucket = "public-data/cache/gbif-app"
) {
  label <- "inat-hexes"
  hash <- digest::digest(list(poly, zoom, id_column, label))
  s3 <- glue::glue("s3://{bucket}/{label}/{hash}.geojson")

  if (!is_cached(s3)) {
    message(paste("opening inat at zoom", zoom))
    inat <- open_inat_area(
      poly = poly,
      zoom = zoom,
      id_column = id_column,
      taxa_selections = taxa_selections
    )

    message("counting species richness by hex")
    inat <- inat |>
      dplyr::count(h3id) |>
      dplyr::mutate(logn = log(n), value = logn / max(logn)) |>
      dplyr::mutate(
        geom = ST_GeomFromText(
          h3_cell_to_boundary_wkt(h3id)
        )
      )

    ## this part should be separate? Or be included in cache logic.

    duckdbfs::to_geojson(inat, s3, as_http = TRUE)
  }

  url <- gsub("^s3://", glue::glue("{protocol}://{server}/"), s3)
  print(url)

  url
}


get_inat_table <- function(
  poly,
  zoom = 4L,
  id_column = "id",
  taxa_selections = list(),
  server = Sys.getenv("AWS_S3_ENDPOINT", "minio.carlboettiger.info"),
  bucket = "public-data/cache/gbif-app"
) {
  ## this part should be separate? Or be included in cache logic.
  label <- "inat"
  hash <- digest::digest(list(poly, zoom, id_column, label))
  s3 <- glue::glue("s3://{bucket}/{label}/{hash}.geparquet")

  message(paste("opening inat at zoom", zoom))
  inat <- open_inat_area(
    poly = poly,
    zoom = zoom,
    id_column = id_column,
    taxa_selections = taxa_selections
  )

  inat
}


get_inat_zonal <- function(
  poly,
  zoom = 4L,
  id_column = "id",
  taxa_selections = list(),
  server = Sys.getenv("AWS_S3_ENDPOINT", "minio.carlboettiger.info"),
  protocol = http_protocol(),
  bucket = "public-data/cache/gbif-app"
) {
  label <- "inat"
  hash <- digest::digest(list(poly, zoom, id_column, label))
  s3 <- glue::glue("s3://{bucket}/{label}/{hash}.geojson")

  if (!is_cached(s3)) {
    # check if hash exists first

    message(paste("opening inat at zoom", zoom))
    inat <- open_inat_area(
      poly = poly,
      zoom = zoom,
      id_column = id_column,
      taxa_selections = taxa_selections
    )

    message("counting species richness by polygon")
    inat <- inat |>
      dplyr::count(.data[[id_column]]) |>
      dplyr::mutate(logn = log(n), value = logn / max(logn))

    # join back to poly with geoms
    poly <- poly |>
      dplyr::select(dplyr::all_of(id_column), geometry) |>
      dplyr::inner_join(inat, by = id_column) |>
      rename(geom = "geometry")

    ## this part should be separate? Or be included in cache logic.

    duckdbfs::to_geojson(poly, s3, as_http = TRUE)
  }

  gsub("^s3://", glue::glue("{protocol}://{server}/"), s3)
}

# FIXME do the filter!
filter_inat_taxa <- function(df, selections = list()) {
  taxa <- open_dataset(
    "s3://public-inat/taxonomy/taxa_and_common.parquet",
    recursive = FALSE
  )
  ranks <- colnames(taxa)

  # If no selections made, return original dataset
  if (length(selections) == 0) {
    return(df)
  }

  # Start with the original dataset

  # Apply filters for each selected taxonomic rank
  for (rank in names(selections)) {
    if (rank %in% ranks) {
      taxa <- taxa |>
        dplyr::filter(.data[[rank]] == !!selections[[rank]])
    }
  }

  taxa |> select(taxon_id = id) |> inner_join(df)
}

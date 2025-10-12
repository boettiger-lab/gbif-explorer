open_gbif_partition <- function(subset, server) {
  if (length(subset) < 1) {
    return(duckdbfs::open_dataset(
      glue::glue("s3://public-gbif/hex/"),
      tblname = "gbif"
    ))
  }

  urls <- paste0(
    glue::glue("https://{server}/public-gbif/hex/h0="),
    subset,
    "/part0.parquet"
  )
  gbif <- open_dataset(urls, tblname = "gbif")
}


open_gbif_region <- function(poly_hexed, server) {
  subset <- poly_hexed |>
    dplyr::distinct(h0) |>
    dplyr::pull()
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


# group by species instead of spatial unit
richness_table <- function(
  poly,
  zoom,
  id_column = "id",
  taxa_selections = list(),
  server = Sys.getenv("AWS_S3_ENDPOINT", "minio.carlboettiger.info")
) {
  poly_hexed_url <- get_h3_aoi(poly, precision = zoom, keep_cols = id_column)
  poly_hexed <- duckdbfs::open_dataset(poly_hexed_url, recursive = FALSE)

  open_gbif_region(poly_hexed, server) |>
    filter_gbif_taxa(taxa_selections) |>
    select(
      kingdom,
      phylum,
      class,
      order,
      family,
      genus,
      species,
      taxonkey,
      !!index
    ) |>
    inner_join(poly_hexed) |>
    distinct() |>
    count(kingdom, phylum, class, order, family, genus, species, taxonkey)
}


# compute zonal stats.
# benchmark pure spatial approaches vs hex-joins
# What resolution should we hex vs when should we switch to st_contains?
get_zonal_richness <- function(
  poly,
  zoom,
  id_column = "id",
  taxa_selections = list(),
  server = Sys.getenv("AWS_S3_ENDPOINT", "minio.carlboettiger.info"),
  local = FALSE
) {
  gbif_stats <- get_zonal_richness_(
    poly,
    zoom,
    id_column,
    taxa_selections,
    server
  )

  # join on id_column to poly
  poly <- poly |>
    dplyr::select(dplyr::all_of(id_column), geometry) |>
    dplyr::inner_join(gbif_stats, by = id_column) |>
    rename(geom = "geometry")

  materialize(
    poly,
    zoom = zoom,
    id_column = id_column,
    local = local,
    max_features = getOption("shiny_max_features", 20000L),
    bucket = "public-data/cache/gbif-app",
    label = "richness",
    server = "minio.carlboettiger.info"
  )
}

get_zonal_richness_ <- function(
  poly,
  zoom,
  id_column = "id",
  taxa_selections = list(),
  server
) {
  poly_hexed_url <- get_h3_aoi(poly, precision = zoom, keep_cols = id_column)
  poly_hexed <- duckdbfs::open_dataset(poly_hexed_url, recursive = FALSE)

  hash <- digest::digest(list(poly_hexed_url, taxa_selections))
  cache <- paste0(
    "s3://public-data/cache/gbif-app/zonal_richness/",
    hash,
    fileext = ".parquet"
  )

  if (!is_cached(cache)) {
    # zoom is already determined by poly_hexed
    hexcols <- poly_hexed |> colnames()
    index <- hexcols[2]
    id_col <- hexcols[3]

    open_gbif_region(poly_hexed, server) |>
      filter_gbif_taxa(taxa_selections) |>
      dplyr::select(taxonkey, !!index) |>
      dplyr::inner_join(poly_hexed) |>
      dplyr::distinct() |>
      dplyr::count(.data[[id_col]]) |>
      dplyr::mutate(logn = log(n), value = logn / max(logn)) |>
      duckdbfs::write_dataset(cache)
  }

  open_dataset(cache, recursive = FALSE)
}


# Hex-based calculation of species richness
get_richness_ <- function(poly, zoom, id_column, taxa_selections, server) {
  poly_hexed_url <- get_h3_aoi(poly, precision = zoom, keep_cols = id_column)
  poly_hexed <- duckdbfs::open_dataset(poly_hexed_url, recursive = FALSE)

  hash <- digest::digest(list(poly_hexed_url, taxa_selections))
  cache <- paste0(
    "s3://public-data/cache/gbif-app/richness/",
    hash,
    fileext = ".parquet"
  )

  if (!is_cached(cache)) {
    print(paste("no cache at", cache, "computing..."))
    # zoom is already determined by poly_hexed
    hexcols <- poly_hexed |> colnames()
    index <- hexcols[2]

    open_gbif_region(poly_hexed, server) |>
      filter_gbif_taxa(taxa_selections) |>
      dplyr::select(taxonkey, !!index) |>
      dplyr::inner_join(poly_hexed) |>
      dplyr::distinct() |>
      dplyr::rename(h3id = !!index) |>
      dplyr::count(h3id) |>
      dplyr::mutate(logn = log(n), value = logn / max(logn)) |>
      dplyr::mutate(
        geom = ST_GeomFromText(
          h3_cell_to_boundary_wkt(h3id)
        )
      ) |>
      duckdbfs::write_dataset(cache)
  }

  duckdbfs::open_dataset(cache, recursive = FALSE)
}

get_richness <- function(
  poly,
  zoom,
  id_column = "id",
  taxa_selections = list(),
  max_features = getOption("shiny_max_features", 20000L),
  warning = TRUE,
  verbose = TRUE,
  server = Sys.getenv("AWS_S3_ENDPOINT", "minio.carlboettiger.info"),
  local = FALSE
) {
  if (verbose) {
    print(paste(
      "Computing biodiversity for",
      digest::digest(poly),
      "at zoom",
      as.integer(zoom),
      "for taxa:",
      paste(taxa_selections, collapse = ":")
    ))
  }

  # main compute task uses S3-cached data
  gbif <- get_richness_(
    poly = poly,
    zoom = zoom,
    id_column = id_column,
    taxa_selections = taxa_selections,
    server = server
  )

  # in-memory gdf will crash above a certain number of hexes
  if (warning) {
    n_features <- gbif |> count() |> pull(n)
    print(paste("computed", n_features, "hexes"))
    if (n_features > max_features) {
      warning(paste("returning only first", max_features, "of", n_features))
    }
  }
  materialize(
    gbif,
    zoom = zoom,
    id_column = id_column,
    local = local,
    max_features = max_features,
    bucket = "public-data/cache/gbif-app",
    label = "richness",
    server = "minio.carlboettiger.info"
  )
}

materialize <- function(
  gdf,
  zoom,
  id_column,
  local = FALSE,
  max_features = getOption("shiny_max_features", 20000L),
  bucket = "public-data/cache/gbif-app",
  label = "richness",
  server = "minio.carlboettiger.info"
) {
  if (is.null(local)) {
    return(gdf)
  }

  if (local) {
    gdf <- gdf |>
      head(max_features) |> # max number of features
      duckdbfs::to_sf(crs = 4326)
  } else {
    hash <- digest::digest(list(gdf, zoom, id_column, label))
    s3 <- glue::glue("s3://{bucket}/{label}/{hash}.geojson")

    gdf |> duckdbfs::to_geojson(s3)
    gdf <- gsub("s3://", glue::glue("https://{server}/"), s3)
  }
  gdf
}

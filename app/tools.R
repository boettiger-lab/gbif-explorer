SERVER <- Sys.getenv(
  "AWS_PUBLIC_ENDPOINT",
  Sys.getenv("AWS_S3_ENDPOINT", "minio.carlboettiger.info")
)
get_carbon <- function(
  poly_hexed,
  taxa_selections = list()
) {}

# group by species instead of spatial unit
richness_table <- function(
  poly,
  zoom,
  id_column = "id",
  taxa_selections = list(),
  server = SERVER
) {
  poly_hexed <- get_h3_aoi(poly, precision = zoom, keep_cols = id_column)

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

# should this also support richness or single species?
inat_rangemap <- function(
  poly,
  zoom,
  id_column = "id",
  taxa_selections = list()
) {
  poly_hexed <- get_h3_aoi(poly, precision = zoom, keep_cols = id_column)
}


# compute zonal stats.
# benchmark pure spatial approaches vs hex-joins
# What resolution should we hex vs when should we switch to st_contains?
get_zonal_richness <- function(
  poly,
  zoom,
  id_column = "id",
  taxa_selections = list(),
  server = SERVER
) {
  poly_hexed <- get_h3_aoi(poly, precision = zoom, keep_cols = id_column)
  gbif_stats <- get_zonal_richness_(poly_hexed, taxa_selections, server)
}

get_zonal_richness_ <- function(
  poly_hexed,
  taxa_selections = list(),
  server = SERVER
) {
  hash <- digest::digest(list(poly_hexed, taxa_selections))
  cache <- paste0(
    "s3://public-data/gbif-cache/zonal_richness/",
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
      select(taxonkey, !!index) |>
      inner_join(poly_hexed) |>
      distinct() |>
      count(!!id_col) |>
      mutate(logn = log(n), value = logn / max(logn)) |>
      mutate(geom = h3_cell_to_boundary_wkt(h3id)) |>
      write_dataset(cache)
  }

  open_dataset(cache, recursive = FALSE)
}


# Hex-based calculation of species richness
get_richness_ <- function(
  poly_hexed,
  taxa_selections = list(),
  server = SERVER,
  cache_path = "s3://public-data/gbif-cache/richness/"
) {
  hash <- digest::digest(list(poly_hexed, taxa_selections))
  cache <- paste0(
    cache_path,
    hash,
    fileext = ".parquet"
  )

  if (!is_cached(cache)) {
    print("no cache, computing...")
    # zoom is already determined by poly_hexed
    hexcols <- poly_hexed |> colnames()
    index <- hexcols[2]

    open_gbif_region(poly_hexed, server) |>
      filter_gbif_taxa(taxa_selections) |>
      select(taxonkey, !!index) |>
      inner_join(poly_hexed) |>
      distinct() |>
      rename(h3id = !!index) |>
      count(h3id) |>
      mutate(logn = log(n), value = logn / max(logn)) |>
      mutate(geom = h3_cell_to_boundary_wkt(h3id)) |>
      write_dataset(cache)
  }

  open_dataset(cache, recursive = FALSE)
}

get_richness <- function(
  poly,
  zoom,
  taxa_selections = list(),
  max_features = getOption("shiny_max_features", 20000L),
  warning = TRUE,
  verbose = TRUE,
  server = SERVER
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

  ## This step is memoised (after materializing the poly)
  poly_hexed <- get_h3_aoi(poly, as.integer(zoom))

  # main compute task uses S3-cached data
  gbif <- get_richness_(
    poly_hexed = poly_hexed,
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

  gbif <- gbif |>
    head(max_features) |> # max number of features
    collect() |>
    st_as_sf(wkt = "geom", crs = 4326)

  gbif
}

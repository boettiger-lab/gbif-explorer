server <- Sys.getenv("AWS_PUBLIC_ENDPOINT", Sys.getenv("AWS_S3_ENDPOINT"))

get_carbon <- function(
  poly_hexed,
  taxa_selections = list()
) {}

# list by species
richness_table <- function() {}

# should this also support richness or single species?
inat_rangemap <- function() {}


# compute zonal stats.
# benchmark pure spatial approaches vs hex-joins
# What resolution should we hex vs when should we switch to st_contains?
get_zonal_richness <- function(
  poly,
  zoom,
  id_column = "id",
  taxa_selections = list(),
  server = server
) {
  poly_hexed <- get_h3_aoi(poly, precision = zoom, keep_cols = id_column)
}

get_zonal_richness_ <- function(
  poly_hexed,
  taxa_selections = list(),
  server = server
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

  cache
}


# Hex-based calculation of species richness
get_richness_ <- function(
  poly_hexed,
  taxa_selections = list(),
  server = server
) {
  hash <- digest::digest(list(poly_hexed, taxa_selections))
  richness_cache <- paste0(
    "s3://public-data/gbif-cache/richness/",
    hash,
    fileext = ".parquet"
  )

  if (!is_cached(richness_cache)) {
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

  # main compute task uses S3-cached data
  richness_cache <- get_richness_(
    poly_hexed = poly_hexed,
    taxa_selections = taxa_selections,
    server
  )

  gbif <- open_dataset(richness_cache, recursive = FALSE)

  # in-memory gdf will crash above a certain number of hexes
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

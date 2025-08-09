library(dplyr)
library(duckdbfs)
library(sf)
library(glue)
duckdbfs::load_h3()

# assumes geom column is "geom"
get_h3_aoi <- function(aoi, precision = 6L) {
  index <- as.integer(0L) # index for h0-partitioned data

  # consider auto-retry at higher precision if subset is empty.
  precision <- as.integer(precision)

  res <- paste0("h", precision)
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
    as_view("h3_aoi")
}


open_gbif_partition <- function(
  subset,
  server = Sys.getenv("AWS_PUBLIC_ENDPOINT", Sys.getenv("AWS_S3_ENDPOINT"))
) {
  urls <- paste0(
    glue("https://{server}/public-gbif/hex/h0="),
    subset,
    "/part0.parquet"
  )
  gbif <- open_dataset(urls, tblname = "gbif")
}

open_gbif_region <- function(
  poly_hexed,
  zoom,
  cache = uuid::uuid(),
  server = Sys.getenv("AWS_PUBLIC_ENDPOINT", Sys.getenv("AWS_S3_ENDPOINT"))
) {
  subset <- poly_hexed |> distinct(h0) |> pull()
  gbif <- open_gbif_partition(subset, server)

  return(gbif)
}

filter_gbif_taxa <- function(gbif, taxa_selections) {
  selections <- taxa_selections$selections()

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


get_richness <- function(
  poly,
  zoom,
  taxa_selections = list(),
  cache = uuid::uuid(),
  local_cache = tempfile("richness_calc", fileext = ".parquet"),
  max_features = getOption("shiny_max_features", 20000L),
  warning = TRUE,
  server = Sys.getenv("AWS_PUBLIC_ENDPOINT", Sys.getenv("AWS_S3_ENDPOINT"))
) {
  poly_hexed <- get_h3_aoi(poly, as.integer(zoom))

  gbif <- open_gbif_region(poly_hexed, zoom, cache, server)
  #gbif <- filter_gbif_taxa(gbif, taxa_selections)

  index <- paste0("h", zoom)
  timer <- bench::bench_time({
    gbif |>
      select(taxonkey, h3id = !!index) |>
      inner_join(poly_hexed, by = "h3id") |>
      distinct() |>
      count(h3id) |>
      mutate(logn = log(n), value = logn / max(logn)) |>
      mutate(geom = h3_cell_to_boundary_wkt(h3id)) |>
      write_dataset(local_cache)

    gbif <- open_dataset(local_cache)

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
  })

  print(timer)
  gbif
}

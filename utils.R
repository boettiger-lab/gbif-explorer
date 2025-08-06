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

get_richness <- function(
  poly,
  zoom,
  cache = uuid::uuid(),
  server = Sys.getenv("AWS_PUBLIC_ENDPOINT", Sys.getenv("AWS_S3_ENDPOINT"))
) {
  poly_hexed <- get_h3_aoi(poly, as.integer(zoom))
  subset <- poly_hexed |> distinct(h0) |> pull()
  gbif <- open_gbif_partition(subset, server)

  index <- paste0("h", zoom)
  timer <- bench::bench_time({
    gdf <- gbif |>
      select(species, genus, family, order, class, phylum, h3id = !!index) |>
      inner_join(poly_hexed, by = "h3id") |>
      distinct() |>
      count(h3id) |>
      mutate(logn = log(n), value = logn / max(logn)) |>
      mutate(geom = h3_cell_to_boundary_wkt(h3id)) |>
      collect() |>
      st_as_sf(wkt = "geom", crs = 4326)
  })

  print(timer)
  gdf
}

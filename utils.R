library(dplyr)
library(duckdbfs)
library(sf)
duckdbfs::load_h3()

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


hex_res <- function(x) {
  x |>
    utils::head(1) |>
    dplyr::mutate(res = h3_get_resolution(h3id)) |>
    dplyr::pull(res)
}

# join by miss-matched hex resolution
hex_join <- function(x, y) {
  res_x <- hex_res(x)
  res_y <- hex_res(y)

  if (res_x > res_y) {
    y <- y |>
      dplyr::mutate(
        h3id = unnest(h3_cell_to_children(h3id, {
          res_x
        })),
        h3id = toupper(h3id)
      )
  }
  if (res_x < res_y) {
    y <- y |>
      dplyr::mutate(
        h3id = h3_cell_to_parent(h3id, {
          res_x
        })
      )
  }

  dplyr::inner_join(x, y)
}

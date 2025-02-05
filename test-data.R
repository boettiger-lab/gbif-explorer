library(duckdbfs)
library(dplyr)
library(sf)
library(spData)

duckdbfs::load_h3()
duckdbfs::load_spatial()


#fs::file_delete(tmp)
ex1 <- spData::us_states |>  dplyr::filter(NAME == "Arizona")
ex2 <- world |> filter(iso_a2 == "US")


as_dataset.sf <- function(sf, ...) {
  # cludgy way to get polygon into duckdb as spatial data
  tmp <- tempfile(fileext = ".fgb")
  sf |> st_transform(4326) |> write_sf(tmp, append = FALSE)
  aoi <- open_dataset(tmp, ...)

  aoi
}

get_h3index <- function(aoi, zoom = 0L, precision = 6L) {

  zoom <- as.integer(zoom)

  # consider auto-retry at higher precision if subset is empty.
  precision <- as.integer(precision)

  res <- paste0("h", precision)
  # multipolygon dump may not be needed for draw tools.
  h3_aoi <- aoi |>
    mutate(poly = array_extract(unnest(st_dump(geom)),"geom"),
          hexid = h3_polygon_wkt_to_cells(poly,{precision}),
          hexid = unnest(hexid)
          ) |>
    mutate(h0 = h3_h3_to_string( h3_cell_to_parent(hexid, {zoom})),
           hexid = h3_h3_to_string (hexid) ) |>
    mutate(h0 = toupper(h0), hexid = toupper(hexid))

  # create a view as well
  h3_aoi |> select(h0, hexid) |> 
  #rename(!!res := hexid) |> 
  as_view("h3_aoi")

  subset <- h3_aoi |>
    select(h0) |>
    distinct() |>
    pull(h0)

  subset
}

aoi <- as_dataset.sf(ex1)
subset <- get_h3index(aoi)
urls <- paste0("https://minio.carlboettiger.info/public-gbif/hex/h0=", subset, "/part0.parquet")
gbif <- open_dataset(urls, tblname = "gbif")


x <- gbif |> rename(hexid = h8) |> count(hexid, name = "count")

con <- cached_connection()
y <- tbl(con, "h3_aoi")


hex_join <- function(x,y) {
  res_x <- x |> head(1) |> mutate(res = h3_get_resolution(hexid)) |> pull(res)
  res_y <- y |> head(1) |> mutate(res = h3_get_resolution(hexid)) |> pull(res)


  if (res_x > res_y) {
    y <- y |> 
    mutate(hexid = unnest(
          h3_cell_to_children(hexid, {res_x})),
          hexid = toupper(hexid)
          )

  }

    if (res_x < res_y) {
    y <- y |> 
      mutate(hexid = h3_cell_to_parent(hexid, {res_x}))
  }

  inner_join(x, y)

}
hex_join(x,y)

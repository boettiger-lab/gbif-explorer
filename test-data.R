library(duckdbfs)
library(dplyr)
library(sf)
library(spData)
source("utils.R")

duckdb_secrets()
poly <- open_dataset(
  "s3://public-overturemaps/countries.parquet",
  recursive = FALSE
) |>
  filter(primary == "United States") |>
  select(id, division_id, geom = geometry)


bench::bench_time({
  # 2.85s
  poly_h3 <- get_h3_aoi(poly, 3) |> collect()
})


bench::bench_time({
  #14 s
  poly_h5 <- get_h3_aoi(poly, 5) |> collect()
})


bench::bench_time({
  #3.4 m
  poly_h6 <- get_h3_aoi(poly, 6) |> collect()
})

subset <- poly_h3 |> distinct(h0) |> pull()
subset2 <- poly_h5 |> distinct(h0) |> pull()

subset3 <- poly_h6 |> distinct(h0) |> pull()


urls <- paste0(
  "https://minio.carlboettiger.info/public-gbif/hex/h0=",
  subset,
  "/part0.parquet"
)
gbif <- open_dataset(urls, tblname = "gbif")

bench::bench_time({
  x <- gbif |> rename(h3id = h5) |> inner_join(poly_h5) |> compute()
})
con <- cached_connection()
y <- tbl(con, "h3_aoi")


hex_join <- function(x, y) {
  res_x <- x |> head(1) |> mutate(res = h3_get_resolution(hexid)) |> pull(res)
  res_y <- y |> head(1) |> mutate(res = h3_get_resolution(hexid)) |> pull(res)

  if (res_x > res_y) {
    y <- y |>
      mutate(
        hexid = unnest(
          h3_cell_to_children(hexid, {
            res_x
          })
        ),
        hexid = toupper(hexid)
      )
  }

  if (res_x < res_y) {
    y <- y |>
      mutate(
        hexid = h3_cell_to_parent(hexid, {
          res_x
        })
      )
  }

  inner_join(x, y)
}
hex_join(x, y)

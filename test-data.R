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
  poly_h3 <- get_h3_aoi(poly, 3)
})


bench::bench_time({
  #14 s
  get_h3_aoi(poly, 5) |> write_dataset("tmp.parquet")
})
poly_h5 <- open_dataset("tmp.parquet")


poly_h5 <- get_h3_aoi(poly, 5)

subset <- poly_h5 |> distinct(h0) |> pull()

urls <- paste0(
  "https://minio.carlboettiger.info/public-gbif/hex/h0=",
  subset,
  "/part0.parquet"
)
gbif <- open_dataset(urls, tblname = "gbif")

bench::bench_time({ # 3.7 s
  gbif |>
   select(species, genus, family, order,
          class, phylum, h3id = h5) |>
    inner_join(poly_h5) |> 
    count(h3id) |>
    write_dataset("richness.parquet")
})

bench::bench_time({ # 5 s
  gbif |>
   select(species, genus, family, order,
          class, phylum, h3id = h5) |>
    inner_join(poly_h5) |> 
    count(species) |>
    write_dataset("species.parquet")
})

bench::bench_time({ # 43 s
  gbif |>
   select(species, genus, family, order,
          class, phylum, h3id = h5) |>
    inner_join(poly_h5) |> 
    write_dataset("selection.parquet")
})



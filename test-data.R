
library(duckdbfs)
library(dplyr)

library(sf)
library(spData)
bounds <- spData::us_states |>  dplyr::filter(NAME == "Arizona") |> sf::st_bbox()
attach(as.list(bounds))


duckdb_secrets(Sys.getenv("MINIO_KEY"),
               Sys.getenv("MINIO_SECRET"),
               "minio.carlboettiger.info")

gbif <- open_dataset("s3://public-gbif/2024-10-01",  tblname = "gbif")
gbif_aoi <- 
        gbif |> dplyr::filter(decimallatitude > 0) |> show_query()





gbif_aoi |> show_query()

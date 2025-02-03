library(duckdb.agent)
library(duckdbfs)
library(dplyr)
library(ellmer)


duckdb_secrets(Sys.getenv("MINIO_KEY"),
               Sys.getenv("MINIO_SECRET"),
               "minio.carlboettiger.info")
gbif <- open_dataset("s3://public-gbif/2024-10-01",  tblname = "gbif")
tracts_url <- "https://minio.carlboettiger.info/public-social-vulnerability/2022-tracts-h3-z8.parquet"
tracts_h3 <- open_dataset(tracts_url, tblname = "censustracts")


system_prompt = create_prompt(additional_instructions = 
"Note that the column h8 contains a geohash representing a H3 hexagon index.
If asked for data that requires both tables, you should always seek to join on 
the h8 column.  Always aggregate results to count the number of rows matching
the query in each h8 hexagon")

agent <- ellmer::chat_vllm(
  base_url = "https://llm.cirrus.carlboettiger.info/v1/",
  model = "kosbu/Llama-3.3-70B-Instruct-AWQ",
  api_key = Sys.getenv("CIRRUS_LLM_KEY"),
  system_prompt = system_prompt,
  api_args = list(temperature = 0)
)

resp <- agent$chat("Birds in Yolo County")
out <- agent_query(resp)

bench::bench_time({
out |> rename(h3id = h6) |> to_h3j("s3://public-data/test5.h3j")
})



library(mapgl)
url = "https://minio.carlboettiger.info/public-data/test5.h3j"
maplibre(center=c(-110, 38), zoom = 3, pitch = 30) |>
  add_h3j_source("h3j_source",
                 url = url
  )  |>
  add_fill_extrusion_layer(
    id = "h3j_layer",
    source = "h3j_source",
    fill_extrusion_color = interpolate(
      column = "count",
      values = c(0, 1000),
      stops = c("#430254", "#f83c70")
    ),
    fill_extrusion_height = list(
      "interpolate",
      list("linear"),
      list("zoom"),
      0,
      0,
      100,
      list("*", 2, list("get", "count"))
    ),
    fill_extrusion_opacity = 0.7
  )


library(duckdbfs)
duckdb_secrets()
landvote_z8 = open_dataset("s3://shared-tpl/landvote/z8/landvote_h3_z8.parquet", recursive = FALSE)
colnames(landvote_z8)

x <- open_dataset("https://huggingface.co/datasets/boettiger-lab/landvote/resolve/main/party_polygons.parquet")

library(dplyr)
library(duckdbfs)
library(mapgl)

pad_tract = open_dataset("https://minio.carlboettiger.info/public-biodiversity/pad-us-4/pad-by-tract.parquet")
pad = open_dataset("https://minio.carlboettiger.info/public-biodiversity/pad-us-4/pad-us-4.parquet")
pmtiles = "https://minio.carlboettiger.info/public-biodiversity/pad-us-4/pad-us-4.pmtiles"

#svi = open_dataset("https://data.source.coop/cboettig/social-vulnerability/2022/SVI2022_US_tract.parquet")


State = "Colorado"
filtered_data = pad_tract |> filter(STATE == State) |> select(Unit_Nm)
full_data = pad |> filter(GAP_Sts %in% c("1", "2"))




filter_column <- function(full_data, filtered_data, id_col) {
  #if (nrow(filtered_data) < 1) return(NULL)
  values <- full_data |>
    inner_join(filtered_data, copy = TRUE) |>
    pull(id_col)
  # maplibre syntax for the filter of PMTiles  
  list("in", list("get", id_col), list("literal", values))
}

maplibre(center = c(-102.9, 41.3), zoom = 3) |>
    add_fill_layer(
        id = "pad",
        source = list(type = "vector", url  = paste0("pmtiles://", pmtiles)),
        source_layer = "padus4",
        tooltip = c("Unit_Nm"),
        # filter = list("in", list("get", "GAP_Sts"), list("literal", values)),
        filter = filter_column(full_data, filtered_data, "row_n"),
        fill_opacity = 0.5,
        fill_color = "darkgreen"
    )


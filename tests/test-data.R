library(duckdbfs)
library(dplyr)
library(sf)
library(spData)
library(mapgl)
source("app/utils.R")
source("app/data-layers.R")
duckdb_secrets()



poly <- open_dataset(
  "s3://public-overturemaps/regions.parquet",
  recursive = FALSE
) |>
  filter(primary == "Nevada") |>
  select(id, division_id, geom = geometry)


poly_hexed <- get_h3_aoi(poly)
get_richness(poly_hexed, list())

# weird things happen hexing too much?
#z = sf::st_sf(sf::st_as_sfc(st_bbox(c(xmin = 180, ymin = -89.99, xmax = 180, ymax = 89.99), crs=4326)))
#get_h3_aoi(z)


dest <- glue::glue("s3://public-data/cache/biodiversity/nevada-all-z5-v1.h3j")
url <- get_richness(poly, 5, dest)


maplibre(center = c(-110, 40.417), zoom = 5, pitch = 40) |>
  add_h3j_source("h3j_source", url = url) |>
  add_fill_extrusion_layer(
    id = "h3j_layer",
    source = "h3j_source",
    tooltip = concat("Richness:", get_column("n")),
    fill_extrusion_color = interpolate(
      column = "value",
      values = c(0, 1),
      stops = c("#430254", "#f83c70")
    ),
    fill_extrusion_height = list("*", 100000, list("get", "value")),
    fill_extrusion_opacity = 0.7
  )

# fancier vertical scaling
  list(
      "interpolate",
      list("linear"),
      4,
      0,
      6,
      list("*", "1000", list("get", "value"))
    ),


url = "https://minio.carlboettiger.info/public-data/cache/biodiversity/991ebc3b-dd31-4cb8-87ec-3ca964db6db4.h3j"
maplibre(center = c(-118, 40.417), zoom = 6) |>
      add_h3j_source("h3j_source", url = url) |>
      add_richness()


maplibre(center = c(-118, 40.417), zoom = 5) |>
  add_h3j_source("h3j_source", url = url) |>
  add_fill_layer(
    id = "h3j_layer",
    source = "h3j_source",
    fill_opacity=0.1,
    tooltip = concat("Richness:", get_column("n")),
    fill_color = interpolate(
      column = "value",
      values = c(0, 1),
      stops = c("#430254", "#f83c70")
    )
  )


#####



poly_h5 <- get_h3_aoi(poly, as.integer(5L))
subset <- poly_h5 |> distinct(h0) |> pull()
gbif <- open_gbif_partition(subset)

	

bench::bench_time({ # 3.7 s
  gbif |>
   select(specieskey, h3id = h5) |>
    inner_join(poly_h5, by = "h3id") |> 
    count(h3id) |>
    write_dataset("occurrence.parquet")
})

bench::bench_time({ 
  gdf <- gbif |>
   select(specieskey, h3id = h5) |>
    inner_join(poly_h5) |> 
    distinct(specieskey,h3id) |>
    count(h3id) |> 
    mutate(geom = h3_cell_to_boundary_wkt(h3id)) |>
    collect() |>
     st_as_sf(wkt = "geom")
})

bench::bench_time({ 
  gbif |>
   select(specieskey, h3id = h5) |>
    inner_join(poly_h5) |> 
    distinct(specieskey,h3id) |>
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



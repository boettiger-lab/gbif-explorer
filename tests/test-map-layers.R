library(mapgl)
source("app/data-layers.R")
MAXZOOM = 9

 m <- mapgl::maplibre(
      zoom = 2,
      center = c(-100, 30),
      maxZoom = MAXZOOM
    )

    m <- m |>
      add_pmtiles_source("tract_source", tract, promoteId = "primary") |>
      add_pmtiles_source("county_source", counties, promoteId = "primary") |>
      add_pmtiles_source("region_source", regions, promoteId = "primary") |>
      add_pmtiles_source("country_source", countries, promoteId = "primary") |>
      add_pmtiles_source("pad_source", pad_us_4, promoteId = "Unit_Nm") |>
      add_hillshade_source()

m |> add_pad()

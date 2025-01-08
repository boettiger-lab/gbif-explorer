---
---


# Vector Layers

The heart of this application design is a vector dataset serialized as both (Geo)Parquet and PMTiles.
The parquet version allows for real-time calculations through rapid SQL queries via duckdb, 
and the PMTiles version allows the data to be quickly visualized at any zoom through maplibre.
maplibre can also efficiently filter the PMTiles data given a feature ids returned by duckdb.

`gdal_translates` can generate both PMTiles and geoparquet, though `tippecanoe` provides more
options for PMTiles generation and can produce nicer tile sets.

The demo uses the CDC Social Vulnerability data because it is built on the hierachical partitioning
used by the Census (Country->State->County->Tract) hierarchy.  

# Raster Layers

## Generating static tiles

## Zonal statistics calculations 

The application is essentially driven by the vector layer data using SQL. 
I find it helpful to pre-process 'zonal' calculations, e.g. the mean value of each raster layer
within each feature in the 'focal' vector data set(s).


# Server-Referenced URLs

This document lists all URLs in the codebase that reference `{server}` and use the configurable `{protocol}` (http/https).

## Data Layers (app/data-layers.R)

### PMTiles Files (Hardcoded - External Access Required)
Note: PMTiles URLs are hardcoded to `https://minio.carlboettiger.info` because they need to be accessible from the browser, not just the server.

- `https://minio.carlboettiger.info/public-overturemaps/countries.pmtiles`
- `https://minio.carlboettiger.info/public-overturemaps/regions.pmtiles`
- `https://minio.carlboettiger.info/public-overturemaps/counties.pmtiles`
- `https://minio.carlboettiger.info/public-social-vulnerability/2022/SVI2022_US_tract.pmtiles`
- `https://minio.carlboettiger.info/public-biodiversity/pad-us-4/pad-us-4.pmtiles`

### Parquet Files (Configurable Protocol)
- `{protocol}://{server}/public-overturemaps/countries.parquet`
- `{protocol}://{server}/public-overturemaps/regions.parquet`
- `{protocol}://{server}/public-overturemaps/counties.parquet`
- `{protocol}://{server}/public-social-vulnerability/2022/svi_tract.parquet`
- `{protocol}://{server}/public-biodiversity/pad-us-4/pad-us-4.parquet`

### Default GDF Sources
- `{protocol}://{server}` (used as default for richness_layer)
- `{protocol}://{server}` (used as default for carbon_layer)

## GBIF Data

### Taxa Files
- `{protocol}://{server}/public-gbif/taxa.parquet` (app/taxa-filter.R)
- `{protocol}://{server}/public-gbif/taxa.parquet` (app/llm-gbif.R)
- `{protocol}://{server}/public-gbif/taxa.parquet` (tests/app-taxa.R)
- `{protocol}://{server}/public-gbif/taxa.parquet` (tests/llm-taxa-filter.R)

### Hex-partitioned GBIF Data
- `{protocol}://{server}/public-gbif/hex/h0={partition}/part0.parquet` (app/tools-richness.R)

## Carbon Data (app/tools-carbon.R)
- `{protocol}://{server}/public-carbon/hex/vulnerable-carbon/h0={partition}/data_0.parquet`

## iNaturalist Data (app/tools-inat.R)
- `{protocol}://{server}/` (prefix for S3 to HTTP/HTTPS conversion)
  - Used to convert S3 paths like `s3://bucket/path` to `{protocol}://{server}/bucket/path`

## Test Data

### PMTiles Files (Hardcoded - External Access Required)
- `https://minio.carlboettiger.info/public-social-vulnerability/2022/SVI2022_US_county.pmtiles` (tests/usa-data-layer.R)
- `https://minio.carlboettiger.info/public-social-vulnerability/2022/SVI2022_US_tract.pmtiles` (tests/usa-data-layer.R)

## Preprocessing (preprocess/preprocess.R)
- `{protocol}://{server}/public-social-vulnerability/2022/SVI2022_US_tract.parquet`
- `{protocol}://{server}/public-biodiversity/pad-us-4/pad-us-4.parquet`

## Protocol Configuration

The `{protocol}` variable is determined by the `http_protocol()` function in `app/utils.R`:
- Returns `"http"` if `AWS_HTTPS` environment variable is set to a truthy value
- Returns `"https"` otherwise (default)

This allows internal Kubernetes services to use HTTP while external access uses HTTPS.

## Server Configuration

The `{server}` variable is typically set to:
- `Sys.getenv("AWS_S3_ENDPOINT")` - primary server endpoint
- `Sys.getenv("AWS_PUBLIC_ENDPOINT", Sys.getenv("AWS_S3_ENDPOINT"))` - for public-facing endpoints
- Default fallback: `"minio.carlboettiger.info"`

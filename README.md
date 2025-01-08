---
title: Geo Llm R
emoji: 📚
colorFrom: blue
colorTo: yellow
sdk: docker
pinned: false
license: bsd-2-clause
---

# Demo Shiny App with Maplibre + open LLM interface

:hugs: Shiny App on Huggingface: <https://huggingface.co/spaces/boettiger-lab/geo-llm-r>

Work in progress.  This is a proof-of-principle for an LLM-driven interface to dynamic mapping. Key technologies include duckdb, geoparquet, pmtiles, maplibre, open LLMs (via VLLM + LiteLLM).  R interface through ellmer (LLMs), mapgl (maplibre), shiny, and duckdb.

# Setup

## GitHub with HuggingFace Deploy

All edits should be pushed to GitHub. Edits to `main` branch are automatically deployed to HuggingFace via GitHub Actions.
When using this scaffold, you will first have to set up your auto-deploy system: 

- [Create a new HuggingFace Space](https://huggingface.co/new-space) (any template is fine, will be overwritten).
- [Create a HuggingFace Token](https://huggingface.co/settings/tokens/new?tokenType=write) with write permissions if you do not have one.  
- In the GitHub Settings of your repository, add the token as a "New Repository Secret" under the `Secrets and Variables` -> `Actions` section of settings (`https://github.com/{USER}/{REPO}/settings/secrets/actions`).  
- Edit the `.github/workflows/deploy.yml` file to specify your HuggingFace user name and HF repo to publish to. 

## Language Model setup

This example is designed to be able to leverage open source or open weights models.  You will need to adjust the API URL and API key accordingly. This could be a local model with `vllm` or `ollama`, and of course commercial models should work too. The demo app currently runs on an VLLM+LiteLLM backed model, currently a Llama3 variant, hosted on the National Research Platform.

The LLM plays only a simple role in generating SQL queries from background information on the data including the table schema, see the system prompt for details. Most open models I have experimented with do not support the [tool use](https://ellmer.tidyverse.org/articles/tool-calling.html) or [structured data](https://ellmer.tidyverse.org/articles/structured-data.html) interfaces very well compared to commercial models.  An important trick in working with open models used here is merely requesting the reply be structured as JSON.  Open models are quite decent at this, and at SQL construction, given necessary context about the data. The map and chart elements merely react the resulting data frames, and the entire analysis is thus transparent and reproducible as it would be if the user had composed their request in SQL instead of plain English. 

## Software Dependencies

The Dockerfile includes all dependencies required for the HuggingFace deployment, and can be used as a template or directly to serve RStudio server.

## Data pre-processing

Pre-processing the data into cloud-native formats and hosting data on a high bandwidth, highly avalialbe server is essential for efficient and scalable renending.  Pre-computing expensive operations such as zonal statistics across all features is also necessary. These steps are described in [preprocess.md](preprocess.md) and corresponding scripts.



FROM rocker/geospatial:latest

WORKDIR /code

RUN apt-get update && apt-get -y install glances
RUN install2.r --error \
  bench \
  bsicons \
  bslib \
  colourpicker \
  conflicted \
  duckdbfs \
  ellmer \
  fontawesome \
  gt \
  mapgl \
  markdown \
  memoise \
  sf \
  shiny \
  shinybusy \
  shinychat \
  tidyverse

RUN R -e "remotes::install_github('cboettig/duckdbfs')"

COPY app/ .

EXPOSE 8080
CMD ["R", "--quiet", "-e", "shiny::runApp(host='0.0.0.0', port=8080)"]


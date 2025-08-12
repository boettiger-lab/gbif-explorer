FROM rocker/geospatial:latest

WORKDIR /code

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
  shiny \
  shinybusy \
  shinychat \
  tidyverse

COPY app/ .

CMD ["R", "--quiet", "-e", "shiny::runApp(host='0.0.0.0', port=7860)"]

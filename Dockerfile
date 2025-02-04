FROM rocker/geospatial:latest

WORKDIR /code

RUN install2.r --error \
    bsicons \
    bslib \
    duckdbfs \
    fontawesome \
    gt \
    markdown \
    shiny \
    shinychat \
    tidyverse \
    shinybusy

RUN installGithub.r cboettig/mapgl tidyverse/ellmer boettiger-lab/duckdb.agent cboettig/duckdbfs

COPY . .

CMD ["R", "--quiet", "-e", "shiny::runApp(host='0.0.0.0', port=7860)"]

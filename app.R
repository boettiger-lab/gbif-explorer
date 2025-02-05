library(shiny)
library(bslib)
library(htmltools)
library(fontawesome)
library(bsicons)
library(bench)
library(glue)
library(sf)
library(duckdb.agent)
library(duckdbfs)
library(dplyr)
library(ellmer)
library(mapgl)
library(digest)
library(stringr)
library(shinybusy)

source("utils.R")

duckdbfs::load_spatial()

css <-
  HTML(paste0("<link rel='stylesheet' type='text/css' ",
              "href='https://demos.creative-tim.com/",
              "material-dashboard/assets/css/",
              "material-dashboard.min.css?v=3.2.0'>"))


# Define the UI
ui <- page_sidebar(
  fillable = FALSE, # do not squeeze to vertical screen space
  tags$head(css),
  titlePanel("Demo App"),
  shinybusy::add_busy_spinner(),
  
  markdown("
  Select a desired area with the draw tools on the map, using the search bar if desired. 
  Then hit **Set Area of Interest** to select.
  Then, enter your query in the text box below the map to count occurrences of your specified taxonomic group.
  Use the airplane button to send your query.  The computation may take a few minutes depending on the size and scale of
  the search.
"),

p("
Scroll to zoom, ctrl+click to pitch and rotate. Hitting the area button with no selection to include the entire map.
"),

  layout_columns(
    card(maplibreOutput("map")),
    div(actionButton("get_features", "Set Area Of Interest", icon = icon("object-group"),
                   class = "btn-primary align-bottom")),
    col_widths = c(11,1)
    ),


  card(
    layout_columns(
      textInput("chat",
        label = NULL,
        "show all bird occurrences at zoom level 6",
        width = "100%"),

      div(
      actionButton("user_msg", "", icon = icon("paper-plane"),
                   class = "btn-primary btn-sm align-bottom"),
      class = "align-text-bottom"),
      col_widths = c(11, 1),
      fill = FALSE
    ),
  ),
  
  textOutput("agent"),

  sidebar = sidebar(
    card(fill = TRUE,
    card_header("Selected area:"),
    verbatimTextOutput("feature_output")
    ),

    selectInput(
    "select",
    "Select an LLM:", 
    list("Llama3.3-cirrus" = "Llama3.3-cirrus")
  ),
    card(fill = TRUE,
      card_header(fa("robot"),  textOutput("model", inline = TRUE)),
      accordion(
        open = TRUE,
        accordion_panel(
          HTML("<span, class='text-info'>Show SQL query</span>"),
          icon = fa("terminal"),
          verbatimTextOutput("sql_code")
        ),
        accordion_panel(
          title = HTML("<span, class='text-info'>Explain query</span>"),
          icon = fa("user", prefer_type = "solid"),
          textOutput("explanation")
        )
      )
    ),
   
   
    card(
      card_header(bs_icon("github"), "Source code:"),
      a(href = "https://github.com/boettiger-lab/biodiversity-justice",
        "https://github.com/boettiger-lab/biodiversity-justice"))
  ),

  theme = bs_theme(version = "5")
)


duckdb_secrets(Sys.getenv("MINIO_KEY"),
            Sys.getenv("MINIO_SECRET"),
            "minio.carlboettiger.info")




    # system prompt generation is slow, do only once??

    system_prompt = create_prompt(additional_instructions =
    "Note that the columns h1, h2, h3, through h11 contains a geohash representing a H3 hexagon index.
    Higher numbers indicate higher zoom resolution (smaller hexes)
    Always aggregate results to count the number of rows matching
    the query to the desired hexagon. Always name the count column 'count'.
    Remember to group by hexagon level to aggregate! 

    Always rename the chosen hexagon column as 'h3id' in your final answer.
    Only select the h3id and count in your final answer. 

    Examples: 
  user: 'show all bird occurrences at zoom level 6'

  your reply: 

    {
    'query': 'CREATE OR REPLACE VIEW bird_occurrences_h6 AS SELECT gbif.h6 AS h3id, COUNT(*) AS count FROM gbif WHERE gbif.class = 'Aves' GROUP BY gbif.h6',
    'table_name': 'bird_occurrences_h6',
    'explanation': 'This query creates a view that shows the count of bird occurrences at zoom level 6. It selects the h6 column as the hexagon id, counts the number of rows for each hexagon, and groups the results by the h6 column.'
    }

    Refer to the full table by its table name as given above.
    Be sure to list column names 
    Be sure to generate fully valid SQL. Check your SQL for possible errors.
    

    Do not use the 'scientificname' column! Instead, filter specific species using the
    binomial name as the 'species' column.

    IMPORTANT: return raw JSON only, do not decorate your reply with markdown code syntax.
    ")





# Define the server
server <- function(input, output, session) {

  # first we draw the map with geosearch and draw controls.
  output$map <- renderMaplibre({
    m <- maplibre(center = c(-110, 38), zoom = 2, pitch = 0, maxZoom = 12) |>
      add_draw_control() |>
      add_geocoder_control()

    m
  })

  # React to user's polygon
  observeEvent(input$get_features, {
    bounds <- ""
    aoi_info <- NULL

    drawn_features <- get_drawn_features(mapboxgl_proxy("map"))
    if(nrow(drawn_features) > 0) {

      aoi <- as_dataset.sf(drawn_features)
      h3_aoi <- get_h3_aoi(aoi)
      subset <- h3_aoi |> distinct(h0) |> pull(h0)


      print(h3_aoi)

      urls <- paste0("https://minio.carlboettiger.info/public-gbif/hex/h0=", subset, "/part0.parquet")
      gbif <- open_dataset(urls, tblname = "gbif")
      # would be better to spatial join
      bounds <- st_bbox(drawn_features)

   #   timer <- bench::bench_time({
   #   xmin <- bounds[1]; ymin <- bounds[2]; xmax <- bounds[3]; ymax <- bounds[4]
   #   open_dataset(urls, tblname = "gbif") |> 
   #     #filter(between(decimallongitude, xmin, xmax), between(decimallatitude, ymin, ymax)) |> 
   #     mutate(geom = st_geomfromwkb(geom)) |> spatial_join(aoi) |>
   #     as_view("gbif_aoi")
   #   })
   #   print(timer)

      output$feature_output <- renderPrint(print(bounds))
    }



  observeEvent(input$user_msg, {

    model <- reactive(input$select)()

    if (grepl("cirrus", model)) {
      agent <- ellmer::chat_vllm(
        base_url = "https://llm.cirrus.carlboettiger.info/v1/",
        model = "kosbu/Llama-3.3-70B-Instruct-AWQ",
        api_key = Sys.getenv("CIRRUS_LLM_KEY"),
        system_prompt = system_prompt,
        api_args = list(temperature = 0)
      )
    } else {
      agent <- ellmer::chat_vllm( # NRP models have too small a context window for useful interaction
        base_url = "https://llm.nrp-nautilus.io/",
        model = model,
        api_key = Sys.getenv("NRP_API_KEY"),
        system_prompt = system_prompt,
        api_args = list(temperature = 0)
      )
    }


    print("Agent thinking...")
    stream <- agent$chat(input$chat)

    # Parse response
    response <- jsonlite::fromJSON(stream)

    if ("query" %in% names(response)) {
      output$sql_code <- renderText(str_wrap(response$query, width = 60))
      output$explanation <- renderText(response$explanation)

    # clear agent memory
    agent$set_turns(NULL)

    } else {
      output$agent <- renderText(response$agent)
    }

      # cache the query
      query_id <- digest::digest(paste(response$query, bounds, collapse=""))
      data_url <- glue::glue("https://minio.carlboettiger.info/public-data/cache/{query_id}.h3j")
      
      # use tempfile as cache.  we could use database tempdir
      cache_parquet <- tempfile(glue("{query_id}"), fileext = ".parquet")




      # compute if not yet in cache
      status <- httr::status_code(httr::HEAD(data_url))
      if(status == 404) {
        print("Computing...")
        time <- bench::bench_time({
          agent_query(stream) |>
          hex_join(h3_aoi) |>
          mutate(log_count = log(count)) |>
          write_dataset(cache_parquet)
        })
        print(time)
      }
      cached_data <- open_dataset(cache_parquet)

      # so we can scale color and height to max value
      biggest <-
        cached_data |> 
        summarise(max = max(log_count)) |>
        pull(max) |>
        first()

      # so we can zoom to the selected data (choose random point)
      aoi_info <- cached_data |>
        head(1) |> 
        mutate(zoom = h3_get_resolution(h3id),
               lat = h3_cell_to_lat(h3id),
               lng = h3_cell_to_lng(h3id)) |>
        collect()
                  

      # draw on map
      h3j <- glue("s3://public-data/cache/{query_id}.h3j")
      cached_data |> to_h3j(h3j)

      # adjust v-scale based on zoom:
      vscale <- 7000 / aoi_info$zoom

      # override previous map with drawn map
      # we should use set_h3j_source and set_layer on maplibre_proxy instead.
      output$map <- renderMaplibre({
          m <- maplibre(center=c(-110, 38), zoom = 1, pitch = 0, maxZoom = 12) |>
          add_h3j_source("h3j_source",
                        url = data_url) |>
          add_fill_extrusion_layer(
            id = "h3j_layer",
            source = "h3j_source",
            tooltip = "count",
            fill_extrusion_color = interpolate(
              column = "log_count",
              values = c(0, biggest),
              stops = c("#430254", "#f83c70")
            ),
            fill_extrusion_height = list(
              "interpolate",
              list("linear"),
              list("zoom"),
              0, 0, biggest,
              list("*", vscale, list("get", "log_count"))
            ),
            fill_extrusion_opacity = 0.7
          )
        if (!is.null(aoi_info)) {
          m <- m |> fly_to(c(aoi_info$lng, aoi_info$lat), zoom = (aoi_info$zoom - 1))
        }

        m
      }) # close renderMaplibre
    }) # close observeEvent->get_features
  }) # close observeEvent->user_msg
}

# Run the app
shinyApp(ui = ui, server = server)
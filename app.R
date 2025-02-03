library(shiny)
library(bslib)
library(htmltools)
library(fontawesome)
library(bsicons)
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

duckdbfs::close_connection()

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
  
  p("
  Select a desired area with the draw tools on the map, then hit 'Set Area of Interest' to select.
  Then, enter your query in the text box below the map to count occurrences of your specified taxonomic group.
  Use the airplane button to sned your query.  The computation may take a few minutes depending on the size and scale of
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
    list("LLama3" = "llama3",
         #"OLMO2 (AllenAI)" = "olmo",
         "Gorilla (UC Berkeley)" = "gorilla" 
        )
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
          title = "Explain query",
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

gbif <- open_dataset("s3://public-gbif/2024-10-01",  tblname = "gbif")
bounds <- ""

# Define the server
server <- function(input, output, session) {
  output$map <- renderMaplibre({
    m <- maplibre(center=c(-110, 38), zoom = 3, pitch = 0) |>
      add_draw_control() |>
      add_geocoder_control() #|> 
      # set_projection("globe")
    
    m
  })
observeEvent(input$get_features, {

      drawn_features <- get_drawn_features(mapboxgl_proxy("map"))
      if(nrow(drawn_features) > 0) {
        bounds <- st_bbox(drawn_features)

        # print(bounds)
        output$feature_output <- renderPrint({
            print(bounds)
        })


        attach(as.list(bounds))
        gbif_aoi <- gbif |>
          dplyr::filter(between(decimallatitude, ymin, ymax),
                      between(decimallongitude, xmin, xmax)) |>
          as_view("gbif_aoi")

      }

  observeEvent(input$user_msg, {



    system_prompt = create_prompt(additional_instructions =
    "Note that the columns h1, h2, h3, through h11 contains a geohash representing a H3 hexagon index.
    Higher numbers indicate higher zoom resolution (smaller hexes)
    Always aggregate results to count the number of rows matching
    the query to the desired hexagon. Always name the count column 'count'.
    Remember to group by hexagon level to aggregate! 
    Always rename the chosen hexagon column as 'h3id' in your final answer.
    Always use table notation like 'gbif.order' to specify column names.
    Be sure to generate fully valid SQL. Check your SQL for possible errors.

    Always use the table 'gbif_aoi' rather than 'gbif' table if both are present.

    IMPORTANT: return raw JSON only, do not decorate your reply with markdown code syntax.
    ")

    agent <- ellmer::chat_vllm(
      base_url = "https://llm.cirrus.carlboettiger.info/v1/",
      model = "kosbu/Llama-3.3-70B-Instruct-AWQ",
      api_key = Sys.getenv("CIRRUS_LLM_KEY"),
      system_prompt = system_prompt,
      api_args = list(temperature = 0)
    )


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
      output$url <- renderText(data_url)
      
      cache_parquet <- glue("{query_id}.parquet")

      # compute if not yet in chache
      status <- httr::status_code(httr::HEAD(data_url))
      if(status == 404) {
        print("Computing...")
        time <- bench::bench_time({
          agent_query(stream) |> write_dataset(cache_parquet)
        })
        print(time)
      }

      # draw on map
      h3j <- glue("s3://public-data/cache/{query_id}.h3j")
      open_dataset(cache_parquet) |> to_h3j(h3j)

      # override previous map with drawn map
      output$map <- renderMaplibre({
          m <- maplibre(center=c(-110, 38), zoom = 3, pitch = 0, maxZoom = 9) |>
          add_h3j_source("h3j_source",
                        url = data_url) |>
          add_fill_extrusion_layer(
            id = "h3j_layer",
            source = "h3j_source",
            tooltip = "count",
            fill_extrusion_color = interpolate(
              column = "count",
              values = c(0, 1000),
              stops = c("#430254", "#f83c70")
            ),
            fill_extrusion_height = list(
              "interpolate",
              list("linear"),
              list("zoom"),
              0, 0, 1000,
              list("*", 10, list("get", "count"))
            ),
            fill_extrusion_opacity = 0.7
          )
      }) # close renderMaplibre
    }) # close observeEvent->get_features
  }) # close observeEvent->user_msg
}

# Run the app
shinyApp(ui = ui, server = server)
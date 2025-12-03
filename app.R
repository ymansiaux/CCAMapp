library(shiny)
library(data.table)
library(DBI)
library(dplyr)
library(DT)
library(duckdb)
library(duckplyr)
library(mapgl)
library(sf)
library(tidyr)
library(shinyjs)
library(bslib)

R_files <- list.files(here::here("R"), full.names = TRUE)
for (file in R_files) {
  source(file)
}


# Define UI for application that draws a histogram
ui <- page_sidebar(
  useShinyjs(),
  title = "Exploration actes CCAM",
  sidebar = sidebar(
    mod_ccam_select_ui("ccam1")
  ),
  tagList(
    fluidRow(
      h2("Actes CCAM sélectionnés"),
      DTOutput("out")
    ),
    fluidRow(mod_filter_open_ccam_ui("filter_open_ccam_1")),
    fluidRow(mod_maps_ui("maps_1"))
  )
)

#   fluidRow(
#     column(6, mod_ccam_select_ui("ccam1")),
#     column(6, DTOutput("out")),
#     column(6, actionButton("erase_selection", "Effacer la sélection"))
#   ),
#   fluidRow(
#     column(6, mod_filter_open_ccam_ui("filter_open_ccam_1"))
#   ),
#   fluidRow(
#     mod_maps_ui("maps_1")
#   )
# )

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  con <- dbConnect(duckdb::duckdb())

  referentiel_actes_csv_path <- file.path(
    here::here("external_data"),
    "referentiel_actes.csv"
  )
  open_ccam_csv_path <- file.path(
    here::here("external_data"),
    "open_ccam/open_ccam_24.csv"
  )

  dbExecute(
    con,
    sprintf(
      "
      CREATE TABLE referentiel_actes AS
      SELECT * FROM read_csv_auto('%s');
      ",
      referentiel_actes_csv_path
    )
  )

  dbExecute(
    con,
    sprintf(
      "
      CREATE TABLE open_ccam AS
      SELECT * FROM read_csv_auto('%s');
      ",
      open_ccam_csv_path
    )
  )

  dept_sf <- sf::read_sf(
    here::here("external_data", "departements.geojson")
  ) %>%
    sf::st_transform(4326)

  all_thematics_csv <- list.files(
    here::here("external_data", "specialites"),
    full.names = TRUE
  )

  all_thematics_codes <- lapply(all_thematics_csv, function(x) {
    suppressWarnings(
      csv_thematique <- data.table::fread(x, quote = '"')
    )
    unlist(csv_thematique[, 1])
  })
  names(all_thematics_codes) <- gsub(".csv", "", basename(all_thematics_csv))

  # Other server logic
  rv <- reactiveValues(
    ccam = NULL,
    filtered_referentiel = NULL
  )

  output$out <- renderDT({
    req(rv$filtered_referentiel)
    DT::datatable(
      rv$filtered_referentiel,
      options = list(pageLength = 10, lengthChange = FALSE)
    )
  })

  observeEvent(input$erase_selection, {
    rv$ccam <- NULL
    rv$filtered_referentiel <- NULL
  })

  mod_ccam_select_server("ccam1", con, rv, all_thematics_codes)

  mod_filter_open_ccam_server("filter_open_ccam_1", rv, con, dept_sf)
  mod_maps_server("maps_1", rv, dept_sf)
}

# Run the application
shinyApp(ui = ui, server = server)

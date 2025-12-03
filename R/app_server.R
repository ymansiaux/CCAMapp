#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import duckplyr
#' @importFrom duckdb duckdb
#' @import DBI
#' @import sf
#' @importFrom DT renderDT
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  # DuckDB
  con <- dbConnect(duckdb::duckdb())

  referentiel_actes_csv_path <- system.file(
    "referentiel_actes.csv",
    package = "CCAMapp"
  )
  open_ccam_csv_path <- system.file(
    "open_ccam/open_ccam_24.csv",
    package = "CCAMapp"
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
    system.file("departements.geojson", package = "CCAMapp")
  ) %>%
    sf::st_transform(4326)

  all_thematics_csv <- list.files(
    system.file("specialites", package = "CCAMapp"),
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

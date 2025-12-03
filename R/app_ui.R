#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom DT DTOutput
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      fluidRow(
        column(6, mod_ccam_select_ui("ccam1")),
        column(6, DTOutput("out")),
        column(6, actionButton("erase_selection", "Effacer la sélection"))
      ),
      fluidRow(
        column(6, mod_filter_open_ccam_ui("filter_open_ccam_1"))
      ),
      fluidRow(
        mod_maps_ui("maps_1")
      )
    ),
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "CCAMapp"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

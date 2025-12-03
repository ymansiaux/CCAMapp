#' maps UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_maps_ui <- function(id) {
  ns <- NS(id)
  tagList(
    textOutput(ns("nombre_d_etablissements")),
    fluidRow(
      column(6, maplibreOutput(ns("map"))),
      column(6, maplibreOutput(ns("map_by_dept")))
    )
  )
}

#' maps Server Functions
#'
#' @noRd
mod_maps_server <- function(id, rv, dept_sf) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$nombre_d_etablissements <- renderText({
      req(rv$swm_etablissements_with_selected_ccam)
      paste0(
        "Nombre d'établissements trouvés avec ces codes CCAM: ",
        nrow(rv$swm_etablissements_with_selected_ccam)
      )
    })

    output$map <- renderMaplibre({
      req(rv$swm_etablissements_with_selected_ccam)

      maplibre(bounds = dept_sf) %>%
        add_circle_layer(
          id = "swm_etablissements_with_selected_ccam",
          source = rv$swm_etablissements_with_selected_ccam,
          popup = "nom_du_compte",
          circle_color = "red",
          circle_stroke_color = "white",
          circle_stroke_width = 1,
          cluster_options = cluster_options(
            cluster_radius = 30,
            color_stops = c("#377eb8", "#4daf4a", "#984ea3"),
            count_stops = c(0, 200, 500),
            circle_blur = 0.2,
            circle_stroke_color = "white",
            circle_stroke_width = 5
          )
        )
    })

    output$map_by_dept <- renderMaplibre({
      req(rv$swm_etablissements_with_selected_ccam)

      count_by_dept <- rv$swm_etablissements_with_selected_ccam %>%
        st_drop_geometry() %>%
        group_by(dept) %>%
        summarise(n_etablissements = n())

      dept_sf_with_count <- left_join(
        dept_sf,
        count_by_dept,
        by = c("code" = "dept")
      ) %>%
        # mutate(n_etablissements = if_else(is.na(n_etablissements), 0, n_etablissements)) %>%
        mutate(
          popup = paste0(
            "Département: ",
            code,
            "<br>Nombre d'établissements: ",
            n_etablissements
          )
        )

      maplibre(bounds = dept_sf_with_count) %>%
        add_fill_layer(
          id = "dept_sf_with_count",
          source = dept_sf_with_count,
          popup = "popup",
          fill_color = interpolate(
            column = "n_etablissements",
            values = c(
              min(count_by_dept$n_etablissements, na.rm = TRUE),
              max(count_by_dept$n_etablissements, na.rm = TRUE)
            ),
            stops = c("yellow", "darkred"),
            na_color = "grey"
          ),
          fill_opacity = 0.7
        ) %>%
        add_legend(
          legend_title = "Nombre d'établissements",
          colors = c("yellow", "darkred"),
          values = c(
            min(count_by_dept$n_etablissements, na.rm = TRUE),
            max(count_by_dept$n_etablissements, na.rm = TRUE)
          )
        )
    })
  })
}

## To be copied in the UI
# mod_maps_ui("maps_1")

## To be copied in the server
# mod_maps_server("maps_1", rv, dept_sf)

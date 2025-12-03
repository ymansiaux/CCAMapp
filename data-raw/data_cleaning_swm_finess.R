library(readxl)
library(dplyr)
library(janitor)
library(data.table)
library(sf)
library(tidyr)

data_clean_swm_base <- function(path) {
  raw_xl <- read_excel(path) %>%
    clean_names()

  swm_base <- raw_xl %>%
    mutate(
      hm = grepl(
        "hopital manager| anesthésie",
        tolower(solutions_hopital_manager)
      )
    ) %>%
    mutate(
      hopital_manager = if_else(hm, "HM", "Other")
    ) %>%
    mutate(
      dept = substr(finess_geographique, 1, 2)
    ) %>%
    select(-hm) %>%
    mutate(across(
      c(
        nombre_de_lits_et_places_mco,
        nombre_de_lits_et_places_ssr,
        nombre_de_lits_et_places_psy
      ),
      function(x) replace_na(x, 0)
    )) %>%
    mutate(
      nombre_de_lits_et_places_total = nombre_de_lits_et_places_mco +
        nombre_de_lits_et_places_ssr +
        nombre_de_lits_et_places_psy
    ) %>%
    mutate(
      has_mco = !is.na(nombre_de_lits_et_places_mco) &
        nombre_de_lits_et_places_mco > 0
    ) %>%
    mutate(
      has_ssr = !is.na(nombre_de_lits_et_places_ssr) &
        nombre_de_lits_et_places_ssr > 0
    ) %>%
    mutate(
      has_psy = !is.na(nombre_de_lits_et_places_psy) &
        nombre_de_lits_et_places_psy > 0
    )
  swm_base
}


data_clean_finess <- function(
  path,
  keep_only_mco_ssr_psy = FALSE,
  keep_only_actifs = TRUE
) {
  t_finess <- data.table::fread(path)

  if (keep_only_actifs) {
    t_finess <- t_finess %>%
      filter(etat == "ACTUEL") %>%
      filter(statut_jur_etat == "O")
  }

  t_finess_clean <- t_finess %>%
    filter(geoloc_legal_projection == "L93_METROPOLE") %>%
    mutate(
      mco = san_med == "OUI" | san_chir == "OUI" | san_obs == "OUI",
      ssr = san_smr == "OUI",
      psy = san_psy == "OUI"
    ) %>%
    mutate(
      forme_juridique = case_when(
        grepl("public", tolower(statut_jur_niv1_lib)) ~ "public",
        grepl("privé", tolower(statut_jur_niv1_lib)) ~ "privé",
        TRUE ~ NA_character_
      )
    )

  if (keep_only_mco_ssr_psy) {
    t_finess_clean <- t_finess_clean %>%
      filter(mco == TRUE | ssr == TRUE | psy == TRUE)
  }

  cols_to_keep <- c(
    "finess",
    "rs",
    "mco",
    "ssr",
    "psy",
    "forme_juridique",
    "geoloc_4326_lat",
    "geoloc_4326_long",
    "ej_finess",
    "et_finess"
  )

  t_finess_clean <- t_finess_clean %>%
    select(all_of(cols_to_keep))

  t_finess_4326 <- t_finess_clean %>%
    st_as_sf(coords = c("geoloc_4326_long", "geoloc_4326_lat")) %>%
    st_set_crs(4326) %>%
    select(finess, mco, ssr, psy, forme_juridique, ej_finess, et_finess) %>%
    mutate(dept = substr(finess, 1, 2))

  t_finess_4326
}


filter_swm_base_by_finess <- function(swm_base, t_finess) {
  finess_actifs <- t_finess[
    etat == "ACTUEL" & statut_jur_etat == "O",
    "finess"
  ]

  finess_with_geoloc <- t_finess[
    etat == "ACTUEL" & statut_jur_etat == "O",
    list(finess, geoloc_4326_long, geoloc_4326_lat)
  ]
  a <- swm_base %>%
    inner_join(finess_actifs, by = c("finess_geographique" = "finess"))
  b <- swm_base %>%
    inner_join(finess_actifs, by = c("finess_juridique" = "finess"))
  bind_rows(a, b) %>%
    distinct() %>%
    inner_join(finess_with_geoloc, by = c("finess_geographique" = "finess")) %>%
    st_as_sf(coords = c("geoloc_4326_long", "geoloc_4326_lat")) %>%
    st_set_crs(4326)
}

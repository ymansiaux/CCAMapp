## code to prepare `DATASET` dataset goes here
# Load and process data using package functions
swm_base <- data_clean_swm_base(
  file.path(
    here::here("data-raw"),
    "data_swm_finess",
    "Base clients HM sur SMS avec statut et activité.xlsx"
  )
)
# Filter SWM data to only include active establishments
finess_raw <- data.table::fread(
  file.path(
    here::here("data-raw"),
    "data_swm_finess",
    "t-finess.csv"
  )
)

swm_cleaned_by_finess_sf <- filter_swm_base_by_finess(swm_base, finess_raw)

usethis::use_data(swm_cleaned_by_finess_sf, overwrite = TRUE)

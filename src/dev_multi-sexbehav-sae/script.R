orderly::orderly_develop_start("dev_multi-sexbehav-sae")

setwd("src/dev_multi-sexbehav-sae/")

analysis_level <- c("MWI" = 5)

admin1_level <- c("MWI" = 1)

stopifnot(iso3 %in% names(analysis_level))
stopifnot(iso3 %in% names(admin1_level))

analysis_level <- analysis_level[iso3]
admin1_level <- admin1_level[iso3]

areas <- read_sf(paste0("depends/", tolower(iso3), "_areas.geojson"))
ind <- read_csv(paste0("depends/", tolower(iso3), "_survey_indicators_sexbehav.csv"))

areas <- select(areas, area_id, area_name, area_level, area_level_label,
                parent_area_id, area_sort_order, center_x, center_y)

areas_model <- filter(areas, area_level == analysis_level)

#' Add area_id for admin1 observation

areas_model <- areas_model %>%
  left_join(
    areas %>%
      st_drop_geometry() %>%
      filter(area_level <= analysis_level) %>%
      spread_areas() %>%
      select(area_id, area_id_aggr = paste0("area_id", admin1_level)),
             by = "area_id"
  )

#' Add an integer index for INLA
areas_model <- areas_model %>%
  arrange(area_sort_order) %>%
  mutate(area_idx = row_number())

#' Create adjacency matrix for INLA
adjM <- spdep::poly2nb(areas_model)
adjM <- spdep::nb2mat(adjM, style = "B", zero.policy = TRUE)
colnames(adjM) <- rownames(adjM)

#' For each indicator, create a dataset containing
#' * Raw district level observations
#' * 'Level 1' aggregated estimate
#' * Spatially smoothed district estimates with BYM2 model
#'
#' Two separate models are fit for age stratified (15-19, 20-24, 25-29) and
#' aggregate 15-24 age group estimate. But it would be preferable to derive
#' the age 15-24 estimate as a weighted aggregation of the 15-19 and 20-24
#' age groups.
#'
#' The way this is written assumes that only a single survey is used and
#' that there is only 1 row per district / indicator / age group stratification.
#' This can be written differently if needed.

indicators <- c("eversex", "sex12m", "sexcohab", "sexnonreg", "sexpaid12m", "sti12m")

df <- crossing(
  indicator = indicators,
  age_group = c("Y015_019", "Y020_024", "Y025_029", "Y015_024"),
  areas_model %>%
    st_drop_geometry() %>%
    select(area_id, area_name, area_idx, area_id_aggr,
           area_sort_order, center_x, center_y)
)

#' Add district observations

filter(ind, estimate > 1) # There's a couple of estimates > 1; unsure how, but use pmin() to constrain

df <- df %>%
  left_join(
    ind %>%
      mutate(
        x_eff = n_eff_kish * pmin(estimate, 1)
      ) %>%
      select(indicator, survey_id, area_id, age_group,
             n_clusters, n_observations, n_eff_kish,
             x_eff, estimate, ci_lower, ci_upper),
    by = c("indicator", "age_group", "area_id")
  )

#' Add aggregate admin1 observations

df <- df %>%
  left_join(
    ind %>%
      select(indicator, area_id_aggr = area_id, age_group,
             n_observations_aggr = n_observations,
             estimate_aggr = estimate,
             ci_lower_aggr = ci_lower,
             ci_upper_aggr = ci_upper),
    by = c("indicator", "age_group", "area_id_aggr")
  )


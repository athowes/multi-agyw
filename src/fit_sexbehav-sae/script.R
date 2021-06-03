orderly::orderly_develop_start("fit_sexbehav-sae")

setwd("src/fit_sexbehav-sae/")

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

#' Fit models

res <- list()

for (i_indicator in indicators) {

  print(i_indicator)

  df_age <- df %>%
    filter(
      indicator == i_indicator,
      age_group %in% c("Y015_019", "Y020_024", "Y025_029")
    ) %>%
    mutate(
      age_group = fct_relevel(age_group, "Y020_024"),
      area_idx.15to19 = ifelse(age_group == "Y015_019", area_idx, NA),
      area_idx.20to24 = ifelse(age_group == "Y020_024", area_idx, NA),
      area_idx.25to29 = ifelse(age_group == "Y025_029", area_idx, NA)
    )

  df_15to24 <- df %>%
    filter(
      indicator == i_indicator,
      age_group == "Y015_024"
    )

  #' family = "xbinomial": binomial model with non-integer counts

  fit_age <- inla(x_eff ~ age_group +
                    f(area_idx.15to19, model = "bym2", graph = adjM) +
                    f(area_idx.20to24, model = "bym2", graph = adjM) +
                    f(area_idx.25to29, model = "bym2", graph = adjM),
                  family = "xbinomial", Ntrials = n_eff_kish, data = df_age,
                  control.predictor = list(link = 1),
                  control.compute = list(config = TRUE))

  fit_15to24 <- inla(x_eff ~ f(area_idx, model = "bym2", graph = adjM),
                     family = "xbinomial", Ntrials = n_eff_kish,
                     data = df_15to24,
                     control.predictor = list(link = 1),
                     control.compute = list(config = TRUE))

  df_age <- df_age %>%
    select(-area_idx.15to19, -area_idx.20to24, -area_idx.25to29) %>%
    bind_cols(
      select(fit_age$summary.fitted.values,
             mean,
             median = `0.5quant`,
             lower = `0.025quant`,
             upper = `0.975quant`)
    )

  df_15to24 <- df_15to24 %>%
    bind_cols(
      select(fit_15to24$summary.fitted.values,
             mean,
             median = `0.5quant`,
             lower = `0.025quant`,
             upper = `0.975quant`)
    )

  res[[i_indicator]] <- bind_rows(df_age, df_15to24)
}

res <- bind_rows(res)

write_csv(res, "smoothed-district-sexbehav.csv", na = "")

res_plot <- res %>%
  rename(
    estimate_raw = estimate,
    estimate_smoothed = mean,
    lower_raw = ci_lower,
    lower_aggr = ci_lower_aggr,
    lower_smoothed = lower,
    upper_raw = ci_upper,
    upper_aggr = ci_upper_aggr,
    upper_smoothed = upper
  ) %>%
  pivot_longer(
    cols = c(starts_with("estimate"), starts_with("lower"), starts_with("upper")),
    names_to = c(".value", "source"),
    names_pattern = "(.*)\\_(.*)"
  ) %>%
  left_join(
    select(areas, area_id),
    by = "area_id"
  ) %>%
  st_as_sf() %>%
  split(.$indicator)

pdf("smoothed-district-sexbehav.pdf", h = 11, w = 8.5)
lapply(res_plot, function(x)
  x %>%
    mutate(
      age_group = fct_relevel(age_group, "Y015_024") %>%
        fct_recode("15-24" = "Y015_024", "15-19" = "Y015_019", "20-24" = "Y020_024", "25-29" = "Y025_029"),
      source = fct_relevel(source, "raw", "smoothed", "aggregate") %>%
        fct_recode("Survey raw" = "raw", "Smoothed" = "smoothed", "Admin 1 aggregate" = "aggr")
    ) %>%
    ggplot(aes(fill = estimate)) +
    geom_sf(size = 0.1) +
    scale_fill_viridis_c(option = "C", label = label_percent()) +
    facet_grid(age_group ~ source) +
    theme_minimal() +
    labs(title = paste0(iso3, ": ", x$indicator[1])) +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold"),
      legend.position = "bottom",
      legend.key.width = unit(4, "lines")
    )
)
dev.off()

#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("aaa_fit_all-dhs-multi-sexbehav-sae", parameters = list(iso3 = "MWI", max_model_id = 9))
# setwd("src/aaa_fit_all-dhs-multi-sexbehav-sae")

analysis_level <- c("CMR" = 2,
                    "KEN" = 2,
                    "LSO" = 1,
                    "MOZ" = 2,
                    "MWI" = 5,
                    "NAM" = 2,
                    "SWZ" = 1,
                    "TZA" = 3,
                    "UGA" = 3,
                    "ZAF" = 2,
                    "ZMB" = 2,
                    "ZWE" = 2)

admin1_level <- c("CMR" = 1,
                  "KEN" = 1,
                  "LSO" = 1,
                  "MOZ" = 1,
                  "MWI" = 1,
                  "NAM" = 1,
                  "SWZ" = 1,
                  "TZA" = 2,
                  "UGA" = 1,
                  "ZAF" = 1,
                  "ZMB" = 1,
                  "ZWE" = 1)

stopifnot(iso3 %in% names(analysis_level))
stopifnot(iso3 %in% names(admin1_level))

analysis_level <- analysis_level[iso3]
admin1_level <- admin1_level[iso3]

areas <- read_sf(paste0("depends/", tolower(iso3), "_areas.geojson"))
ind <- read_csv(paste0("depends/", tolower(iso3), "_survey_indicators_sexbehav.csv"))

#' Append an indicator for 1 - sex12m
#' TODO: Move this earlier in the pipeline with a mutate call
ind <- dplyr::bind_rows(
  ind,
  ind %>%
    filter(indicator == "sex12m") %>%
    mutate(indicator = "nosex12m",
          estimate = 1 - estimate,
          ci_upper_new = 1 - ci_lower,
          ci_lower_new = 1 - ci_upper) %>%
    select(-ci_upper, -ci_lower) %>%
    rename(ci_upper = ci_upper_new,
           ci_lower = ci_lower_new)
)

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

#' Using nosex12m rather than e.g. nosex for now
indicators <- c("nosex12m", "sexcohab", "sexnonreg", "sexpaid12m")

df <- crossing(
  indicator = indicators,
  #' Add in the survey_id to deal with multiple surveys
  survey_id = unique(ind$survey_id),
  age_group = c("Y015_019", "Y020_024", "Y025_029", "Y015_024"),
  areas_model %>%
    st_drop_geometry() %>%
    select(area_id, area_name, area_idx, area_id_aggr,
           area_sort_order, center_x, center_y)
)

#' #' Verify that there are no estimates > 1
#' stopifnot(filter(ind, estimate > 1) %>% nrow() == 0)

#' Relax the above to just set ind > 1 to 1, as well as ind < 0 to 0
#' TODO: Investigate more
ind <- ind %>%
  mutate(estimate = pmin(1, estimate),
         estimate = pmax(0, estimate))

#' Add district observations
#' Note that df here has 528 rows: 33 areas, 4 categories, 4 age groups
df <- df %>%
  left_join(
    ind %>%
      mutate(
        x_eff = n_eff_kish * estimate
      ) %>%
      select(indicator, survey_id, area_id, age_group,
             n_clusters, n_observations, n_eff_kish,
             x_eff, estimate, ci_lower, ci_upper),
    by = c("indicator", "survey_id", "age_group", "area_id")
  )

#' Add indicies for:
#'  * survey (sur_idx)
#'  * age (age_idx)
#'  * category (cat_idx)
#'  * observation (obs_idx)
#'  * survey x category (sur_cat_idx)
#'  * age x category (age_cat_idx)
#'  * space x category (area_idx)
df <- df %>%
  mutate(sur_idx = to_int(survey_id),
         age_idx = to_int(age_group),
         cat_idx = to_int(indicator),
         sur_cat_idx = interaction(sur_idx, cat_idx),
         age_cat_idx = interaction(age_idx, cat_idx),
         #' Not sure if this is the best way to do it for obs_idx
         #' Perhaps it can be added earlier in the pipeline
         obs_idx = to_int(interaction(age_idx, area_idx)),
         #' Likewise probably a better way to do this, but having
         #' separate indices seems useful for Besag model
         area_idx.1 = ifelse(cat_idx == 1, area_idx, NA),
         area_idx.2 = ifelse(cat_idx == 2, area_idx, NA),
         area_idx.3 = ifelse(cat_idx == 3, area_idx, NA),
         area_idx.4 = ifelse(cat_idx == 4, area_idx, NA)) %>%
  arrange(obs_idx)

#' Equal to the number of age groups times the number of areas
stopifnot(
  df$obs_idx %>% max() == length(unique(df$age_group)) * length(unique(df$area_id))
)

#' Specify the models to be fit

#' Model 1: age x category random effects (IID)
formula1 <- x_eff ~ -1 + f(obs_idx, hyper = tau_prior(0.000001)) +
  f(age_cat_idx, model = "iid", constr = TRUE, hyper = tau_prior(0.001))

#' Model 2: age x category random effects (IID), space x category random effects (IID)
formula2 <- x_eff ~ -1 + f(obs_idx, hyper = tau_prior(0.000001)) +
  f(age_cat_idx, model = "iid", constr = TRUE, hyper = tau_prior(0.001)) +
  f(area_idx.1, model = "iid", constr = TRUE, hyper = tau_prior(0.001)) +
  f(area_idx.2, model = "iid", constr = TRUE, hyper = tau_prior(0.001)) +
  f(area_idx.3, model = "iid", constr = TRUE, hyper = tau_prior(0.001)) +
  f(area_idx.4, model = "iid", constr = TRUE, hyper = tau_prior(0.001))

#' Model 3: age x category random effects (IID), space x category random effects (BYM2)
formula3 <- x_eff ~ -1 + f(obs_idx, hyper = tau_prior(0.000001)) +
  f(age_cat_idx, model = "iid", constr = TRUE, hyper = tau_prior(0.001)) +
  f(area_idx.1, model = "bym2", graph = adjM, constr = TRUE, hyper = tau_prior(0.001)) +
  f(area_idx.2, model = "bym2", graph = adjM, constr = TRUE, hyper = tau_prior(0.001)) +
  f(area_idx.3, model = "bym2", graph = adjM, constr = TRUE, hyper = tau_prior(0.001)) +
  f(area_idx.4, model = "bym2", graph = adjM, constr = TRUE, hyper = tau_prior(0.001))

#' Model 4: age x category random effects (IID), survey x category random effects (IID) (grouped)
formula4 <- x_eff ~ -1 + f(obs_idx, hyper = tau_prior(0.000001)) +
  f(age_cat_idx, model = "iid", constr = TRUE, hyper = tau_prior(0.001)) +
  f(sur_idx, model = "iid", group = cat_idx, control.group = list(model = "iid"),
    constr = TRUE, hyper = tau_prior(0.001))

#' Model 5: age x category random effects (IID), space x category random effects (IID),
#' survey x category random effects (IID) (grouped)
formula5 <- x_eff ~ -1 + f(obs_idx, hyper = tau_prior(0.000001)) +
  f(age_cat_idx, model = "iid", constr = TRUE, hyper = tau_prior(0.001)) +
  f(area_idx.1, model = "iid", constr = TRUE, hyper = tau_prior(0.001)) +
  f(area_idx.2, model = "iid", constr = TRUE, hyper = tau_prior(0.001)) +
  f(area_idx.3, model = "iid", constr = TRUE, hyper = tau_prior(0.001)) +
  f(area_idx.4, model = "iid", constr = TRUE, hyper = tau_prior(0.001)) +
  f(sur_idx, model = "iid", group = cat_idx, control.group = list(model = "iid"),
    constr = TRUE, hyper = tau_prior(0.001))

#' Model 6: age x category random effects (IID), space x category random effects (BYM2),
#' survey x category random effects (IID) (grouped)
formula6 <- x_eff ~ -1 + f(obs_idx, hyper = tau_prior(0.000001)) +
  f(age_cat_idx, model = "iid", constr = TRUE, hyper = tau_prior(0.001)) +
  f(area_idx.1, model = "bym2", graph = adjM, constr = TRUE, hyper = tau_prior(0.001)) +
  f(area_idx.2, model = "bym2", graph = adjM, constr = TRUE, hyper = tau_prior(0.001)) +
  f(area_idx.3, model = "bym2", graph = adjM, constr = TRUE, hyper = tau_prior(0.001)) +
  f(area_idx.4, model = "bym2", graph = adjM, constr = TRUE, hyper = tau_prior(0.001)) +
  f(sur_idx, model = "iid", group = cat_idx, control.group = list(model = "iid"),
    constr = TRUE, hyper = tau_prior(0.001))

#' Model 7: age x category random effects (IID), survey x category random effects (AR1)
formula7 <- x_eff ~ -1 + f(obs_idx, hyper = tau_prior(0.000001)) +
  f(age_cat_idx, model = "iid", constr = TRUE, hyper = tau_prior(0.001))+
  f(sur_idx, model = "ar1", group = cat_idx, control.group = list(model = "iid"),
    constr = TRUE, hyper = tau_prior(0.001))

#' Model 8: age x category random effects (IID), space x category random effects (IID),
#' survey x category random effects (AR1)
formula8 <- x_eff ~ -1 + f(obs_idx, hyper = tau_prior(0.000001)) +
  f(age_cat_idx, model = "iid", constr = TRUE, hyper = tau_prior(0.001)) +
  f(area_idx.1, model = "iid", constr = TRUE, hyper = tau_prior(0.001)) +
  f(area_idx.2, model = "iid", constr = TRUE, hyper = tau_prior(0.001)) +
  f(area_idx.3, model = "iid", constr = TRUE, hyper = tau_prior(0.001)) +
  f(area_idx.4, model = "iid", constr = TRUE, hyper = tau_prior(0.001)) +
  f(sur_idx, model = "ar1", group = cat_idx, control.group = list(model = "iid"),
    constr = TRUE, hyper = tau_prior(0.001))

#' Model 9: age x category random effects (IID), space x category random effects (BYM2),
#' survey x category random effects (AR1)
formula9 <- x_eff ~ -1 + f(obs_idx, hyper = tau_prior(0.000001)) +
  f(age_cat_idx, model = "iid", constr = TRUE, hyper = tau_prior(0.001)) +
  f(area_idx.1, model = "bym2", graph = adjM, constr = TRUE, hyper = tau_prior(0.001)) +
  f(area_idx.2, model = "bym2", graph = adjM, constr = TRUE, hyper = tau_prior(0.001)) +
  f(area_idx.3, model = "bym2", graph = adjM, constr = TRUE, hyper = tau_prior(0.001)) +
  f(area_idx.4, model = "bym2", graph = adjM, constr = TRUE, hyper = tau_prior(0.001)) +
  f(sur_idx, model = "ar1", group = cat_idx, control.group = list(model = "iid"),
    constr = TRUE, hyper = tau_prior(0.001))

#' All of the possible models
all_formulas <- parse(text = paste0("list(", paste0("formula", 1:9, collapse = ", "), ")")) %>% eval()
all_models <- paste0("Model ", 1:9) %>% as.list()

#' The subset of all possible fit in this script, as specified by model_ids
if(!is.na(max_model_id)) {
  formulas <- all_formulas[1:max_model_id]
  models <- all_models[1:max_model_id]
} else {
  formulas <- all_formulas
  models <- all_models
}

#' Fit the models
res <- purrr::pmap(
  list(formula = formulas, model = models),
  multinomial_model
)

#' All results into a single dataframe
res_df <- lapply(res, "[[", 1) %>% bind_rows()

write_csv(res_df, "multinomial-smoothed-district-sexbehav.csv", na = "")

#' Simple model comparison
#' Something strange happening with WAIC here, unreasonable orders of magnitude
res_fit <- lapply(res, "[[", 2)
ic_df <- sapply(res_fit, function(fit) c("dic" = fit$dic$dic, "waic" = fit$waic$waic)) %>%
  t() %>%
  round() %>%
  as.data.frame() %>%
  mutate(
    iso3 = iso3,
    model = unlist(models),
    .before = dic
  )

write_csv(ic_df, "information-criteria.csv", na = "")

#' Create plotting data
res_plot <- res_df %>%
  rename(
    estimate_raw = estimate,
    estimate_smoothed = mean
  ) %>%
  pivot_longer(
    cols = c(starts_with("estimate")),
    names_to = c(".value", "source"),
    names_pattern = "(.*)\\_(.*)"
  ) %>%
  left_join(
    select(areas, area_id),
    by = "area_id"
  ) %>%
  st_as_sf() %>%
  split(~indicator + model + ~survey_id)

pdf("multinomial-smoothed-district-sexbehav.pdf", h = 11, w = 8.5)

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
    labs(title = paste0(x$survey_id[1], ": ", x$indicator[1], " (", x$model[1], ")")) +
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

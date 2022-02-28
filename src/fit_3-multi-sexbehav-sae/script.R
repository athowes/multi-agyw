#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("fit_3-multi-sexbehav-sae")
# setwd("src/fit_3-multi-sexbehav-sae")

analysis_level <- multi.utils::analysis_level()
admin1_level <- multi.utils::admin1_level()
iso3 <- multi.utils::priority_iso3()

areas <- readRDS("depends/areas.rds")
ind <- read_csv("depends/survey_indicators_sexbehav.csv")
pop <- read_csv("depends/interpolated_population.csv")

#' If PHIA surveys excluded then filter them out of the raw data
if(!include_phia) {
  ind <- ind %>%
    mutate(type = substr(survey_id, 8, 11)) %>%
    filter(type != "PHIA")
}

#' Set ind$estimate > 1 to 1, as well as ind$estimate < 0 to 0
ind$estimate <- multi.utils::constrain_interval(ind$estimate, lower = 0, upper = 1)

areas <- areas %>%
  select(
    area_id, area_name, area_level, area_level_label,
    parent_area_id, area_sort_order, center_x, center_y
  ) %>%
  mutate(
    iso3 = substr(area_id, 1, 3)
  ) %>%
  #' Add column for analysis level
  left_join(
    data.frame(analysis_level) %>%
      tibble::rownames_to_column("iso3"),
    by = "iso3"
  )

#' Areas at the level of analysis
areas_model <- areas %>%
  filter(area_level == analysis_level) %>%
  #' Add area_id for admin1 observation
  left_join(
    areas %>%
      st_drop_geometry() %>%
      filter(area_level <= analysis_level) %>%
      spread_areas() %>%
      select(area_id, area_id_aggr = paste0("area_id", admin1_level)),
    by = "area_id"
  ) %>%
  #' Add an integer index for INLA
  arrange(area_sort_order) %>%
  mutate(area_idx = row_number())

#' Create adjacency matrix for INLA
adjM <- spdep::poly2nb(areas_model)
adjM <- spdep::nb2mat(adjM, style = "B", zero.policy = TRUE)
colnames(adjM) <- rownames(adjM)

#' Create the scaffolding for the estimates
df <- crossing(
  #' In this model we are using three risk categories (rather than four)
  indicator = c("nosex12m", "sexcohab", "sexnonregplus"),
  #' All of the different years
  year = 1999:2020,
  #' Three age groups
  age_group = c("Y015_019", "Y020_024", "Y025_029"),
  #' The areas in the model
  areas_model %>%
    st_drop_geometry() %>%
    select(area_id, area_name, area_idx, area_id_aggr,
           area_sort_order, center_x, center_y)
)

#' Merge district observations into df
df <- df %>%
  left_join(
    ind %>%
      mutate(
        year = as.integer(substr(survey_id, 4, 7)),
        x_eff = n_eff_kish * estimate
      ) %>%
      select(indicator, survey_id,  year, area_id, age_group,
             n_clusters, n_observations, n_eff_kish,
             x_eff, estimate, ci_lower, ci_upper),
    by = c("indicator", "year", "age_group", "area_id")
  )

#' Add indicies for:
#'  * year (year_idx)
#'  * country (iso3_idx)
#'  * age (age_idx)
#'  * category (cat_idx)
#'  * observation (obs_idx)
#'  * year x category (year_cat_idx)
#'  * country x category (iso3_cat_idx)
#'  * age x category (age_cat_idx)
#'  * space x category (area_cat_idx)
#'  * space x year (area_year_idx)
df <- mutate(df,
    year_idx = multi.utils::to_int(year),
    iso3_idx = multi.utils::to_int(substr(area_id, 1, 3)),
    #' Doing this because want Y015_024 to have ID 4 rather than 2 as it would be otherwise
    age_idx = as.integer(factor(age_group, levels = c("Y015_019", "Y020_024", "Y025_029", "Y015_024"))),
    cat_idx = multi.utils::to_int(indicator),
    year_cat_idx = multi.utils::to_int(interaction(year_idx, cat_idx)),
    iso3_cat_idx = multi.utils::to_int(interaction(iso3_idx, cat_idx)),
    age_cat_idx = multi.utils::to_int(interaction(age_idx, cat_idx)),
    area_cat_idx = multi.utils::to_int(interaction(area_idx, cat_idx)),
    area_year_idx = multi.utils::to_int(interaction(area_idx, year_idx)),
    obs_idx = multi.utils::to_int(interaction(age_idx, area_idx, year_idx)),
    area_idx_copy = area_idx,
    year_idx_copy = year_idx
  ) %>%
  arrange(obs_idx)

#' Model 1:
#' * category random effects (IID)
#' * age x category random effects (IID)
formula1 <- x_eff ~ -1 + f(obs_idx, model = "iid", hyper = tau_fixed(0.000001)) +
  f(cat_idx, model = "iid", constr = TRUE, hyper = tau_pc(x = 0.001, u = 2.5, alpha = 0.01)) +
  f(age_idx, model = "iid", group = cat_idx, control.group = list(model = "iid"),
    constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01))

#' Model 2:
#' * Model 1
#' * space x category random effects (IID)
formula2 <- update(formula1,
  . ~ . + f(area_idx, model = "iid", group = cat_idx, control.group = list(model = "iid"),
            constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01))
)

#' Model 3:
#' * Model 1
#' * space x category random effects (Besag)
formula3 <- update(formula1,
  . ~ . + f(area_idx, model = "besag", graph = adjM, scale.model = TRUE, group = cat_idx,
            control.group = list(model = "iid"), constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01))
)

#' Same structure as aaa_fit_3-multi-sexbehav-sae
#' But year_idx rather than sur_idx
#' Add iso3_idx (IID)

formulas <- list(formula1, formula2, formula3)
models <- list("Model 1", "Model 2", "Model 3")

#' Fit the models

#' Number of Monte Carlo samples
S <- 100

# res <- purrr::pmap(
#   list(formula = formulas, model_name = models, S = S),
#   multinomial_model
# )

res <- multinomial_model(formula1, model_name = "Model 1", S = S)
res <- list(res)

#' Extract the df and the full fitted models
res_df <- lapply(res, "[[", 1) %>% bind_rows()
res_fit <- lapply(res, "[[", 2)

#' Add columns for local DIC, WAIC, CPO and PIT
#' res_df has the 15-24 category too
#' Add columns for local DIC, WAIC, CPO
#' res_df has the 15-24 category too
ic <- lapply(res_fit, function(fit) {
  data.frame(
    local_dic = fit$dic$local.dic,
    local_waic = fit$waic$local.waic,
    local_cpo = fit$cpo$cpo
  )
}) %>%
  bind_rows()

res_df <- bind_cols(res_df, ic)

#' Artefact: Model selection information criteria for multinomial models
#' Some of the entries might be NA where there is missing data (INLA ignores these in its calculations)
ic_df <- res_df %>%
  group_by(model) %>%
  summarise(
    dic = sum(local_dic, na.rm = TRUE),
    dic_se = stats::sd(local_dic, na.rm = TRUE) * sqrt(sum(!is.na(local_dic))),
    waic = sum(local_waic, na.rm = TRUE),
    waic_se = stats::sd(local_waic, na.rm = TRUE) * sqrt(sum(!is.na(local_waic))),
    cpo = sum(local_cpo, na.rm = TRUE),
    cpo_se = stats::sd(local_cpo, na.rm = TRUE) * sqrt(sum(!is.na(local_cpo)))
  )

write_csv(ic_df, "information-criteria.csv", na = "")

#' Artefact: Random effect variance parameter posterior means
variance_df <- map(res_fit, function(fit)
  fit$marginals.hyperpar %>%
    #' Calculate the expectation of the variance
    map_df(function(x) inla.emarginal(fun = function(y) 1/y, x)) %>%
    #' Rename Precision to variance
    rename_all(list(~ str_replace(., "Precision for ", "variance_")))
  ) %>%
  bind_rows() %>%
  #' Some of the models have other hyperparameters (e.g. rho)
  select(starts_with("Variance")) %>%
  #' Sum of variance means
  mutate(total_variance = rowSums(., na.rm = TRUE)) %>%
  #' Create new columns with the percentage variance
  mutate(
    across(
      .cols = starts_with("Variance"),
      .fns = list(percentage = ~ . / total_variance),
      .names = "{fn}_{col}"
    )
  ) %>%
  #' Add model identifier column
  mutate(
    model = unlist(models),
    .before = everything()
  )

write_csv(variance_df, "variance-proportions.csv", na = "")

#' Artefact: Smoothed district indicator estimates for multinomial models
res_df <- res_df %>%
  #' Remove superfluous INLA indicator columns
  select(-ends_with("idx"), -ends_with("idx_copy")) %>%
  #' Make it clear which of the estimates are raw and which are from the model (smoothed)
  rename(
    estimate_raw = estimate,
    ci_lower_raw = ci_lower,
    ci_upper_raw = ci_upper,
    estimate_smoothed = prob_mean,
    median_smoothed = prob_median,
    ci_lower_smoothed = prob_lower,
    ci_upper_smoothed = prob_upper
  ) %>%
  mutate(iso3 = iso3, .before = indicator) %>%
  relocate(model, .before = estimate_smoothed)

write_csv(res_df, "multinomial-smoothed-district-sexbehav.csv", na = "")

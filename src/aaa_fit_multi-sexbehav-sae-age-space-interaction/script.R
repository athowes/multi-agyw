#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("aaa_fit_multi-sexbehav-sae-age-space-interaction", parameters = list(iso3 = "MWI"))
# setwd("src/aaa_fit_multi-sexbehav-sae-age-space-interaction")

sf_use_s2(FALSE)

analysis_level <- multi.utils::analysis_level()
admin1_level <- multi.utils::admin1_level()

stopifnot(iso3 %in% names(analysis_level))
stopifnot(iso3 %in% names(admin1_level))

analysis_level <- analysis_level[iso3]
admin1_level <- admin1_level[iso3]

areas <- read_sf(paste0("depends/", tolower(iso3), "_areas.geojson"))
ind <- read_csv(paste0("depends/", tolower(iso3), "_survey_indicators_sexbehav.csv"))

#' If PHIA surveys excluded then filter them out of the raw data
if(!include_phia) {
  ind <- ind %>%
    mutate(type = substr(survey_id, 8, 11)) %>%
    filter(type != "PHIA")
}

#' Set ind$estimate > 1 to 1, as well as ind$estimate < 0 to 0
ind$estimate <- multi.utils::constrain_interval(ind$estimate, lower = 0, upper = 1)

areas <- select(areas, area_id, area_name, area_level, area_level_label,
                parent_area_id, area_sort_order, center_x, center_y)

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

pdf("areas-check.pdf", h = 5, w = 8)

ggplot(areas_model, aes(fill = iso3)) +
  geom_sf(size = 0.1, colour = scales::alpha("grey", 0.25)) +
  labs(fill = "ISO3") +
  theme_minimal()

dev.off()

#' Create adjacency matrix for INLA
adjM <- spdep::poly2nb(areas_model)
adjM <- spdep::nb2mat(adjM, style = "B", zero.policy = TRUE)
colnames(adjM) <- rownames(adjM)

#' Three or four category version?
if(three_category) {
  indicators <- c("nosex12m", "sexcohab", "sexnonregplus")
} else {
  indicators <- c("nosex12m", "sexcohab", "sexnonreg", "sexpaid12m")

  #' And use only the surveys which contain a specific paid sex question
  available_surveys <- read_csv("depends/available-surveys.csv")

  giftsvar_surveys <- available_surveys %>%
    #' Having trouble doing iso3 == iso3, so use this workaround
    rename(
      iso3_copy = iso3
    ) %>%
    filter(
      giftsvar == 1,
      iso3_copy == iso3
    ) %>%
    pull(survey_id) %>%
    unique()

  ind <- ind %>%
    filter(survey_id %in% giftsvar_surveys)
}

#' Create the scaffolding for the estimates
df <- crossing(
  #' In this model we are using three risk categories (rather than four)
  indicator = indicators,
  #' All of the different surveys
  survey_id = unique(ind$survey_id),
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
        x_eff = n_eff_kish * estimate
      ) %>%
      select(indicator, survey_id, area_id, age_group,
             n_clusters, n_observations, n_eff_kish,
             x_eff, estimate, ci_lower, ci_upper),
    by = c("indicator", "survey_id", "age_group", "area_id")
  )


pdf("data-check.pdf", h = 11, w = 8.75)

df %>%
  left_join( #' Use this to make it an sf again
    select(areas, area_id),
    by = "area_id"
  ) %>%
  st_as_sf() %>%
  split(.$survey_id) %>%
  lapply(function(x) {
    x %>%
      ggplot(aes(fill = estimate)) +
      geom_sf(size = 0.1, colour = scales::alpha("grey", 0.25)) +
      scale_fill_viridis_c(option = "C", label = label_percent(), limits = c(0, 1)) +
      facet_grid(indicator ~ age_group) +
      labs(title = paste0(x$survey_id[1])) +
      theme_minimal()
  })

dev.off()

#' Add indicies for:
df <- df %>%
  mutate(
    #' survey
    sur_idx = multi.utils::to_int(survey_id),
    #' age
    age_idx = multi.utils::to_int(age_group),
    #' category
    cat_idx = multi.utils::to_int(indicator),
    #' survey x category
    sur_cat_idx = multi.utils::to_int(interaction(sur_idx, cat_idx)),
    #' age x category
    age_cat_idx = multi.utils::to_int(interaction(age_idx, cat_idx)),
    #' space x category
    area_cat_idx = multi.utils::to_int(interaction(area_idx, cat_idx)),
    #' space x survey
    area_sur_idx = multi.utils::to_int(interaction(area_idx, sur_idx)),
    #' observation
    obs_idx = multi.utils::to_int(interaction(age_idx, area_idx, sur_idx)),
    #' copies
    area_idx_copy = area_idx,
    sur_idx_copy = sur_idx
  ) %>%
  arrange(obs_idx)

#' Specify the models to be fit

#' Baseline model:
#' * category random effects (IID)
#' * age x category random effects (IID)
formula_baseline <- x_eff ~ -1 + f(obs_idx, model = "iid", hyper = multi.utils::tau_fixed(0.000001)) +
  f(cat_idx, model = "iid", constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01)) +
  f(age_idx, model = "iid", group = cat_idx, control.group = list(model = "iid"),
    constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01))

#' Is more than one survey?
include_temporal <- (length(unique(df$survey_id)) > 1)

#' Prior for the correlation parameter of the AR1 model together with the grouped precision parameter
#' For the correlation parameter, we choose a base model of correlation one with P(rho > 0 = 0.75)
ar1_group_prior <- list(
  rho = list(rho = "pc.cor1", param = c(0, 0.75)),
  prec = list(prec = "pc.prec", param = c(2.5, 0.01), initial = log(0.001))
)

#' Model 4:
#' * space x category random effects (Besag)
#' * survey x category random effects (AR1)
formula4 <- update(formula_baseline,
  . ~ . + f(area_idx, model = "besag", graph = adjM, scale.model = TRUE, group = cat_idx,
            control.group = list(model = "iid"), constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01)) +
          f(sur_idx, model = "ar1", group = cat_idx, control.group = list(model = "iid"),
            constr = TRUE, hyper = ar1_group_prior)
)

#' Model 4 extended (Chris thesis revision suggestion)
#' * age x space x category random effects (IID)
# formula4_extended <- update(formula4,
#   . ~ . + f(area_idx, model = "besag", graph = adjM, scale.model = TRUE, group = cat_idx,
#             control.group = list(model = "iid"), constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01)) +
# )

#' Fit the models

#' Number of Monte Carlo samples
S <- 1000

formulas <- list(formula4)
models <- list("Model 4")

#' tryCatch version for safety
try_multinomial_model <- function(...) {
  return(tryCatch(multinomial_model(...), error = function(e) {
    message("Error!")
    return(NULL)
  }))
}

res <- purrr::pmap(
  list(formula = formulas, model_name = models, S = S),
  try_multinomial_model
)

#' Extract the fitted models
res_fit <- lapply(res, "[[", 1)

#' Add columns for local DIC, WAIC, CPO
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
  ) %>%
  mutate(iso3 = iso3, .before = dic)

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
  #' Add model identifier and country columns
  mutate(
    iso3 = iso3,
    model = unlist(models),
    .before = everything()
  )

write_csv(variance_df, "variance-proportions.csv", na = "")

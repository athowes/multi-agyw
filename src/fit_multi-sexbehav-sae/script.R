#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("fit_multi-sexbehav-sae", parameters = list(lightweight = TRUE, fewer_countries = TRUE))
# setwd("src/fit_multi-sexbehav-sae")

analysis_level <- multi.utils::analysis_level()
admin1_level <- multi.utils::admin1_level()

areas <- readRDS("depends/areas.rds")
ind <- read_csv("depends/survey_indicators_sexbehav.csv")

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

#' If working with a subset of the countries for model development and testing, also filter out of the data
if(fewer_countries) {
  subset_iso3 <- c("BWA", "MOZ", "MWI", "ZMB", "ZWE")

  ind <- ind %>%
    mutate(iso3 = substr(survey_id, 1, 3)) %>%
    #' Mostly arbitrary which countries included in the subset
    #' I've gone for something spatially contiguous and across cultural boundaries
    filter(iso3 %in% subset_iso3) %>%
    select(-iso3)

  areas <- filter(areas, iso3 %in% subset_iso3)
}

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
    filter(giftsvar == 1) %>%
    pull(survey_id) %>%
    unique()

  ind <- ind %>%
    filter(survey_id %in% giftsvar_surveys)
}

#' Create the scaffolding for the estimates
df <- crossing(
  indicator = indicators,
  #' All of the different years
  year = 1999:2018,
  #' Three age groups
  age_group = c("Y015_019", "Y020_024", "Y025_029"),
  #' The areas in the model
  areas_model %>%
    st_drop_geometry() %>%
    select(area_id, area_name, area_idx, area_sort_order, center_x, center_y)
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

#' Check that all the surveys are into df
#' (The +1 is for the NA that's included in df but not ind)
length(unique(df$survey_id)) == length(unique(ind$survey_id)) + 1

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

#' Add indices for:
df <- mutate(df,
    #' year
    year_idx = multi.utils::to_int(year),
    #' country
    iso3_idx = multi.utils::to_int(substr(area_id, 1, 3)),
    #' age
    age_idx = multi.utils::to_int(age_group),
    #' category
    cat_idx = multi.utils::to_int(indicator),
    #' year x category
    year_cat_idx = multi.utils::to_int(interaction(year_idx, cat_idx)),
    #' country x category
    iso3_cat_idx = multi.utils::to_int(interaction(iso3_idx, cat_idx)),
    #' age x category
    age_cat_idx = multi.utils::to_int(interaction(age_idx, cat_idx)),
    #' space x category
    area_cat_idx = multi.utils::to_int(interaction(area_idx, cat_idx)),
    #' space x year
    area_year_idx = multi.utils::to_int(interaction(area_idx, year_idx)),
    #' age x country
    age_iso3_idx = multi.utils::to_int(interaction(age_idx, iso3_idx)),
    #' observation
    obs_idx = multi.utils::to_int(interaction(age_idx, area_idx, year_idx)),
    #' copies
    area_idx_copy = area_idx,
    year_idx_copy = year_idx
  ) %>%
  arrange(obs_idx)

#' Baseline model:
#' * category random effects (IID)
#' * age x country x category random effects (IID)
#' * country x category random effects (IID)
formula_baseline <- x_eff ~ -1 + f(obs_idx, model = "iid", hyper = tau_fixed(0.000001)) +
  f(cat_idx, model = "iid", constr = TRUE, hyper = tau_pc(x = 0.001, u = 2.5, alpha = 0.01)) +
  f(age_iso3_idx, model = "iid", group = cat_idx, control.group = list(model = "iid"),
    constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01)) +
  f(iso3_idx, model = "iid", group = cat_idx, control.group = list(model = "iid"),
    constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01))

#' Model 1:
#' * space x category random effects (IID)
#' * year x category random effects (IID)
formula1 <- update(formula_baseline,
  . ~ . + f(area_idx, model = "iid", group = cat_idx, control.group = list(model = "iid"),
            constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01)) +
          f(year_idx, model = "iid", group = cat_idx, control.group = list(model = "iid"),
            constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01))
)

#' Model 1x:
#' * Model 1
#' * space x year x category random effects (IID x IID)
formula1x <- update(formula1,
  . ~ . + f(area_year_idx, model = "iid", group = cat_idx, control.group = list(model = "iid"),
            constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01))
)

#' Model 2:
#' * space x category random effects (Besag)
#' * year x category random effects (IID)
formula2 <- update(formula_baseline,
  . ~ . + f(area_idx, model = "besag", graph = adjM, scale.model = TRUE, group = cat_idx,
            control.group = list(model = "iid"), constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01)) +
          f(year_idx, model = "iid", group = cat_idx, control.group = list(model = "iid"),
            constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01))
)

#' Create Besag x IID interaction adjacency matrix
#' Check the resulting matrix with image()
interaction_adjM_2x <- multi.utils::repeat_matrix(adjM, n = length(unique(df$year_idx)))

#' Model 2x:
#' * Model 2
#' * space x year x category random effects (Besag x IID)
formula2x <- update(formula2,
  . ~ . + f(area_year_idx, model = "besag", graph = interaction_adjM_6x, scale.model = TRUE, group = cat_idx,
            control.group = list(model = "iid"), constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01))
)

#' Prior for the correlation parameter of the AR1 model together with the grouped precision parameter
#' For the correlation parameter, we choose a base model of correlation one with P(rho > 0 = 0.75)
ar1_group_prior <- list(
  rho = list(rho = "pc.cor1", param = c(0, 0.75)),
  prec = list(prec = "pc.prec", param = c(2.5, 0.01), initial = log(0.001))
)

#' Model 3:
#' * space x category random effects (IID)
#' * year x category random effects (AR1)
formula3 <- update(formula_baseline,
  . ~ . + f(area_idx, model = "iid", group = cat_idx, control.group = list(model = "iid"),
            constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01)) +
          f(year_idx, model = "ar1", group = cat_idx, control.group = list(model = "iid"),
            constr = TRUE, hyper = ar1_group_prior)
)

#' Model 3x:
#' * Model 3
#' * space x year x category random effects (IID x AR1)
formula3x <- update(formula3,
  . ~ . + f(area_idx_copy, model = "iid", group = year_idx, replicate = cat_idx,
            control.group = list(model = "ar1", hyper = list(rho = list(prior = "pc.cor1", param = c(0, 0.75)))),
            constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01))
)

#' Model 4:
#' * space x category random effects (Besag)
#' * year x category random effects (AR1)
formula4 <- update(formula_baseline,
  . ~ . + f(area_idx, model = "besag", graph = adjM, scale.model = TRUE, group = cat_idx,
            control.group = list(model = "iid"), constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01)) +
          f(year_idx, model = "ar1", group = cat_idx, control.group = list(model = "iid"),
            constr = TRUE, hyper = ar1_group_prior)
)

#' Model 4x:
#' * Model 4
#' * space x year x category random effects (Besag x AR1)
formula4x <- update(formula4,
  . ~ . + f(area_idx_copy, model = "besag", graph = adjM, scale.model = TRUE, group = year_idx, replicate = cat_idx,
            control.group = list(model = "ar1", hyper = list(rho = list(prior = "pc.cor1", param = c(0, 0.75)))),
            constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01))
)

formulas <- list(formula1, formula1x, formula2, formula2x, formula3, formula3x, formula4, formula4x)
models <- list("Model 1", "Model 1x", "Model 2", "Model 2x", "Model 3", "Model 3x", "Model 4", "Model 4x")

#' Fit the models

#' If low on computational resources i.e. not on the cluster
#' Just fit one model
if(lightweight) {
  formulas <- list(formula4)
  models <- list("Model 4")
}

#' tryCatch version for safety
try_multinomial_model <- function(...) {
  return(tryCatch(multinomial_model(...), error = function(e) {
    message("Error!")
    return(NULL)
  }))
}

res <- purrr::pmap(
  list(formula = formulas, model_name = models),
  try_multinomial_model
)

#' Extract the df and the full fitted models
res_df <- lapply(res, "[[", 1) %>% bind_rows()
res_fit <- lapply(res, "[[", 2)

#' Artefact: Fitted model objects
saveRDS(res_fit, "multi-sexbehav-sae-fits.rds")

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
variance_df <- tryCatch(
  map(res_fit, function(fit)
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
    ),
  error = function(e) {
    message("Error!")
    return(NULL)
  }
)

write_csv(variance_df, "variance-proportions.csv", na = "")

#' Artefact: Smoothed district indicator estimates for multinomial models
res_df <- res_df %>%
  mutate(iso3 = substr(area_id, 1, 3), .before = year) %>%
  #' Make it clear which of the estimates are raw and which are from the model (smoothed)
  rename(
    estimate_raw = estimate,
    ci_lower_raw = ci_lower,
    ci_upper_raw = ci_upper,
    estimate_smoothed = prob_mean
    # median_smoothed = prob_median,
    # ci_lower_smoothed = prob_lower,
    # ci_upper_smoothed = prob_upper
  ) %>%
  relocate(model, .before = estimate_smoothed)

write_csv(res_df, "multi-sexbehav-sae.csv", na = "")

#' Create plotting data for years with surveys
unique_surveys <- res_df %>%
  pull(survey_id) %>%
  unique()

res_plot <- res_df %>%
  filter(paste0(iso3, year) %in% substr(unique_surveys, 1, 7)) %>%
  pivot_longer(
    cols = c(starts_with("estimate")),
    names_to = c(".value", "source"),
    names_pattern = "(.*)\\_(.*)"
  ) %>%
  left_join( #' Use this to make it an sf again
    select(areas, area_id),
    by = "area_id"
  ) %>%
  st_as_sf()

#' Artefact: Cloropleths
pdf("multi-sexbehav-sae.pdf", h = 6, w = 12)

res_plot %>%
  filter(paste0(iso3, year) %in% substr(unique_surveys, 1, 7)) %>%
  #' Can't split on survey_id because missing entries have missing survey_id
  split(~iso3 + year + model) %>%
  lapply(function(x)
    #' For country years with no survey
    if(nrow(x) != 0) {
    x %>%
      ggplot(aes(fill = estimate)) +
      geom_sf(size = 0.1, colour = scales::alpha("grey", 0.25)) +
      scale_fill_viridis_c(option = "C", label = label_percent()) +
      facet_grid(source ~ indicator + age_group) +
      theme_minimal() +
      labs(
        title = paste0(x$survey_id[1], " (", x$model[1], ")")
      ) +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        legend.position = "bottom",
        legend.key.width = unit(4, "lines")
      )
    }
  )

dev.off()

#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("fit_3-multi-sexbehav-sae", parameters = list(include_interactions = TRUE))
# setwd("src/fit_3-multi-sexbehav-sae")

analysis_level <- c("BWA" = 2,
                    "CMR" = 2,
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

admin1_level <- c("BWA" = 1,
                  "CMR" = 1,
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

iso3 <- names(analysis_level)

areas <- readRDS("depends/areas.rds")
ind <- read_csv("depends/survey_indicators_sexbehav.csv")
pop <- read_csv("depends/interpolated_population.csv")

#' Set ind$estimate > 1 to 1, as well as ind$estimate < 0 to 0
ind$estimate <- constrain_interval(ind$estimate, lower = 0, upper = 1)

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

#' Country level area (not to be included in model)
country <- areas %>%
  filter(area_level == 0) %>%
  mutate(
    area_idx = NA,
    area_id_aggr = NA
  )

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
  #' Three age groups, plus aggregate category
  age_group = c("Y015_019", "Y020_024", "Y025_029", "Y015_024"),
  #' Both the areas in the model and the aggregate country
  bind_rows(areas_model, country) %>%
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

#' Merge age-stratified population total sizes into df
#' This is required for aggregating the estimates e.g. using 15-19 and 20-24 to create 15-24
df <- df %>%
  mutate(
    #' Assuming the survey_id is structured as ISO2000DHS
    year = as.numeric(substr(survey_id, 4, 7))
  ) %>%
  left_join(
    pop %>%
      filter(sex == "female") %>%
      select(area_id, year, age_group, population),
    by = c("area_id", "year", "age_group")
  ) %>%
  rename(population_mean = population)

#' Add indicies for:
#'  * year (year_idx)
#'  * age (age_idx)
#'  * category (cat_idx)s
#'  * observation (obs_idx)
#'  * year x category (year_cat_idx)
#'  * age x category (age_cat_idx)
#'  * space x category (area_cat_idx)
#'  * space x year (area_year_idx)
df <- df %>%
  mutate(
    year_idx = to_int(year),
    #' Doing this because want Y015_024 to have ID 4 rather than 2 as it would be otherwise
    age_idx = as.integer(factor(age_group, levels = c("Y015_019", "Y020_024", "Y025_029", "Y015_024"))),
    cat_idx = to_int(indicator),
    year_cat_idx = to_int(interaction(year_idx, cat_idx)),
    age_cat_idx = to_int(interaction(age_idx, cat_idx)),
    area_cat_idx = to_int(interaction(area_idx, cat_idx)),
    area_year_idx = to_int(interaction(area_idx, year_idx)),
    obs_idx = to_int(interaction(age_idx, area_idx, year_idx)),
    area_idx_copy = area_idx,
    year_idx_copy = year_idx
  ) %>%
  arrange(obs_idx)

#' Data for the model (df) doesn't include the aggregates (since this is using data twice)
#' So we save them off separately
df_agg <- df %>%
  filter(age_group == "Y015_024" | area_id %in% iso3)

#' The rows of df to be included in the model
#' Note that setdiff behaving strangely here
df_model <- df %>%
  filter(
    age_group != "Y015_024",
    !(area_id %in% iso3)
  )

#' Check that the rows in the full is a sum of that in the model and aggregate
stopifnot(
  nrow(df) == nrow(df_model) + nrow(df_agg)
)

#' Model 1:
#' * category random effects (IID)
#' * age x category random effects (IID)
formula1 <- x_eff ~ -1 + f(obs_idx, model = "iid", hyper = tau_fixed(0.000001)) +
  f(cat_idx, model = "iid", constr = TRUE, hyper = tau_pc(x = 0.001, u = 2.5, alpha = 0.01)) +
  f(age_idx, model = "iid", group = cat_idx, control.group = list(model = "iid"),
    constr = TRUE, hyper = tau_pc(x = 0.001, u = 2.5, alpha = 0.01))

#' Fit the models

#' Number of Monte Carlo samples
S <- 100

models <- "Model 1"
res <- multinomial_model(formula1, model_name = "Model 1", S = S)

res <- list(res)

#' Extract the df and the full fitted models
res_df <- lapply(res, "[[", 1) %>% bind_rows()
res_fit <- lapply(res, "[[", 2)

#' Add columns for local DIC, WAIC, CPO and PIT
#' res_df has the 15-24 category too
res_df <- bind_cols(
  res_df,
  lapply(res_fit,
         function(fit) {
           return(data.frame(
             local_dic = fit$dic$local.dic,
             local_waic = fit$waic$local.waic,
             local_cpo = fit$cpo$cpo,
             local_pit = fit$cpo$pit
           ))
         }
  ) %>%
    bind_rows() %>%
    #' Being safe here and explictly adding the NA entires for df_agg
    bind_rows(data.frame(
      local_dic = rep(NA, length(models) * nrow(df_agg)),
      local_waic = rep(NA, length(models) * nrow(df_agg)),
      local_cpo = rep(NA, length(models) * nrow(df_agg)),
      local_pit = rep(NA, length(models) * nrow(df_agg))
    ))
)

#' Artefact: Model selection information criteria for multinomial models
#' Some of the entries might be NA where there is missing data (INLA ignores these in its calculations)
ic_df <- sapply(res_fit, function(fit) {
  local_dic <- na.omit(fit$dic$local.dic)
  local_waic <- na.omit(fit$waic$local.waic)
  local_cpo <- na.omit(fit$cpo$cpo)
  local_pit <- na.omit(fit$cpo$pit)

  c("dic" = sum(local_dic),
    "dic_se" = stats::sd(local_dic) * sqrt(length(local_dic)),
    "waic" = sum(local_waic),
    "waic_se" = stats::sd(local_waic) * sqrt(length(local_waic)),
    "cpo" = sum(local_cpo),
    "cpo_se" = stats::sd(local_cpo) * sqrt(length(local_cpo)),
    "pit" = sum(local_pit),
    "pit_se" = stats::sd(local_pit) * sqrt(length(local_pit)))
}) %>%
  t() %>%
  round() %>%
  as.data.frame() %>%
  mutate(
    model = unlist(models),
    .before = dic
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
  #' Add model identifier and country columns
  mutate(
    #' TODO: Needs to be updated for interaction models
    model = paste("Model", row_number()),
    .before = everything()
  )

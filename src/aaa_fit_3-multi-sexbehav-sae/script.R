#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("aaa_fit_3-multi-sexbehav-sae", parameters = list(iso3 = "MWI", include_interactions = TRUE))
# setwd("src/aaa_fit_3-multi-sexbehav-sae")

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

stopifnot(iso3 %in% names(analysis_level))
stopifnot(iso3 %in% names(admin1_level))

analysis_level <- analysis_level[iso3]
admin1_level <- admin1_level[iso3]

areas <- read_sf(paste0("depends/", tolower(iso3), "_areas.geojson"))
ind <- read_csv(paste0("depends/", tolower(iso3), "_survey_indicators_sexbehav.csv"))
pop <- read_csv("depends/interpolated-population.csv")

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
  #' All of the difference surveys
  survey_id = unique(ind$survey_id),
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
        x_eff = n_eff_kish * estimate
      ) %>%
      select(indicator, survey_id, area_id, age_group,
             n_clusters, n_observations, n_eff_kish,
             x_eff, estimate, ci_lower, ci_upper),
    by = c("indicator", "survey_id", "age_group", "area_id")
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
#'  * survey (sur_idx)
#'  * age (age_idx)
#'  * category (cat_idx)
#'  * observation (obs_idx)
#'  * survey x category (sur_cat_idx)
#'  * age x category (age_cat_idx)
#'  * space x category (area_cat_idx)
#'  * space x survey (area_sur_idx)
df <- df %>%
  mutate(
    sur_idx = multi.utils::to_int(survey_id),
    #' Doing this because want Y015_024 to have ID 4 rather than 2 as it would be otherwise
    age_idx = as.integer(factor(age_group, levels = c("Y015_019", "Y020_024", "Y025_029", "Y015_024"))),
    cat_idx = multi.utils::to_int(indicator),
    sur_cat_idx = multi.utils::to_int(interaction(sur_idx, cat_idx)),
    age_cat_idx = multi.utils::to_int(interaction(age_idx, cat_idx)),
    area_cat_idx = multi.utils::to_int(interaction(area_idx, cat_idx)),
    area_sur_idx = multi.utils::to_int(interaction(area_idx, sur_idx)),
    #' Is this the best way to add obs_idx? Perhaps can be added earlier in the pipeline
    obs_idx = multi.utils::to_int(interaction(age_idx, area_idx, sur_idx)),
    area_idx_copy = area_idx,
    sur_idx_copy = sur_idx
  ) %>%
  arrange(obs_idx)

#' Data for the model (df) doesn't include the aggregates (since this is using data twice)
#' So we save them off separately
df_agg <- df %>%
  filter(age_group == "Y015_024" | area_id == toupper(iso3))

#' The rows of df to be included in the model
df_model <- df %>%
  filter(age_group != "Y015_024", area_id != toupper(iso3))

#' Check that the rows in the full is a sum of that in the model and aggregate
stopifnot(
  nrow(df) == nrow(df_model) + nrow(df_agg)
)

#' Specify the models to be fit

#' # Gaussian Markov Kronecker random fields
#'
#' INLA's group argument allows specifying Gaussian Kronecker product random fields with
#' covariance given as the Kronecker product of between group and within group covariance matrices.
#' See https://becarioprecario.bitbucket.io/inla-gitbook/ch-temporal.html#sec:spacetime.
#' Within-group is controlled by f(), and between group is controlled by the control.group
#' argument. Often the group argument is used to define spatiotemporal covariance structures.
#' A spatiotemporal model is called separable when space-time covariance structure can be
#' written as a Kronecker product of a spatial and temporal covariance.
#'
#' Following e.g. Blangiardo and Cameletti (2015), let delta_it be spatio-temporal interaction
#' random effects. Knorr-Held (2000) present four ways to specify the structure matrix R_delta,
#' where in the following R_space and R_time refer to spatially or temporally structured random
#' effects and I_space and I_time unstructured random effects:
#' * Type I: I_space (x) I_time e.g. `f(spacetime, model = "iid")` or seems possible to use group option either way
#' * Type II: I_space (x) R_time e.g. `f(space, model = "iid", group = time, control.group = list(model = "rw1"))`
#' * Type III: R_space (x) I_time e.g. `f(time, model = "iid", group = space, control.group = list(model = "besag"))`
#' * Type IV: R_space (x) R_time e.g. `f(space, model = "besag", group = time, control.group = list(model = "rw1"))`
#'
#' Rather than using the group option to define the spatiotemporal covariance, we use it here to define
#' temporal random effects (indexed by the survey identifier, survey_idx) for each of the multinomial
#' categories. In this case, setting `f(sur_idx)` with `group = cat_idx` gives the grouped random effects:
#'
#' [e(cat 1, time 1), e(cat 1, time 2), e(cat 1, time 3)]
#' [e(cat 2, time 1), e(cat 2, time 2), e(cat 2, time 3)]
#' [e(cat 3, time 1), e(cat 3, time 2), e(cat 3, time 3)]
#'
#' # Additional constraints
#'
#' The interaction random effects should be constrained such that the sum over the non-category index is zero.
#' For example, in each category the sum over ages of \alpha_{ak} should be zero:
#'
#' \sum_a \alpha_{ak} = 0 \forall k = 1, ..., K
#'
#' Intuition for this constraint is as follows. Suppose that the sum over age groups in category k is non-zero.
#'
#' \sum_a \alpha_{a1} = C_1
#' ...
#' \sum_a \alpha_{ak} = C_k
#' ...
#' \sum_a \alpha_{aK} = C_K
#'
#' If C_1 =/= C_k =/= C_K, then the age x category interactions have the effect of increasing the likelihood
#' of a particular category. This isn't the desired effect: increasing the likelihood of any of the categories
#' relative to the others should be left to the category random effects \beta_k.
#'
#' Additional linear constraints may be enforced on random effects in `R-INLA` using
#'
#' `extraconstr = list(A = A, e = e)`
#'
#' See https://becarioprecario.bitbucket.io/inla-gitbook/ch-INLAfeatures.html#sec:constraints.
#' `A` should be a matrix which has `ncol(A) = length(u)` and `nrow(A)` equal to the number of constraints required.
#' `e` should have length equal to the number of constraints required.

#' For some random effects, if the group structure is correctly specified, `extraconstr` are not needed, and
#' the default `constr = TRUE` can be sufficient.

#' Model 1: category random effects (IID), age x category random effects (IID)
formula1 <- x_eff ~ -1 + f(obs_idx, model = "iid", hyper = multi.utils::tau_fixed(0.000001)) +
  f(cat_idx, model = "iid", constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01)) +
  f(age_idx, model = "iid", group = cat_idx, control.group = list(model = "iid"),
    constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01))

#' Model 2: category random effects (IID), age x category random effects (IID),
#' space x category random effects (IID)
formula2 <- update(formula1,
  . ~ . + f(area_idx, model = "iid", group = cat_idx, control.group = list(model = "iid"),
            constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01))
)

#' Model 3: category random effects (IID), age x category random effects (IID),
#' space x category random effects (Besag)
formula3 <- update(formula1,
  . ~ . + f(area_idx, model = "besag", graph = adjM, scale.model = TRUE, group = cat_idx,
            control.group = list(model = "iid"), constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01))
)

formulas <- list(formula1, formula2, formula3)
models <- list("Model 1", "Model 2", "Model 3")

#' If there is more than one survey, then add temporal random effect models
include_temporal <- (length(unique(df_model$survey_id)) > 1)

if(include_temporal) {

  #' Model 4:  category random effects (IID), age x category random effects (IID),
  #' survey x category random effects (IID)
  formula4 <- update(formula1,
    . ~ . + f(sur_idx, model = "iid", group = cat_idx, control.group = list(model = "iid"),
              constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01))
  )

  #' Model 5:  category random effects (IID), age x category random effects (IID),
  #' space x category random effects (IID), survey x category random effects (IID)
  formula5 <- update(formula1,
    . ~ . + f(area_idx, model = "iid", group = cat_idx, control.group = list(model = "iid"),
              constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01)) +
            f(sur_idx, model = "iid", group = cat_idx, control.group = list(model = "iid"),
              constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01))
  )

  #' Model 6: category random effects (IID), age x category random effects (IID),
  #' space x category random effects (Besag), survey x category random effects (IID)
  formula6 <- update(formula1,
    . ~ . + f(area_idx, model = "besag", graph = adjM, scale.model = TRUE, group = cat_idx,
              control.group = list(model = "iid"), constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01)) +
            f(sur_idx, model = "iid", group = cat_idx, control.group = list(model = "iid"),
              constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01))
  )

  #' Prior for the correlation parameter of the AR1 model together with the grouped precision parameter
  #' For the correlation parameter, we choose a base model of correlation one with P(rho > 0 = 0.75)
  ar1_group_prior <- list(
    rho = list(rho = "pc.cor1", param = c(0, 0.75)),
    prec = list(prec = "pc.prec", param = c(2.5, 0.01), initial = log(0.001))
  )

  #' Model 7:  category random effects (IID), age x category random effects (IID),
  #' survey x category random effects (AR1)
  formula7 <- update(formula1,
   . ~ . + f(sur_idx, model = "ar1", group = cat_idx, control.group = list(model = "iid"),
             constr = TRUE, hyper = ar1_group_prior)
  )

  #' Model 8:  category random effects (IID), age x category random effects (IID),
  #' space x category random effects (IID), survey x category random effects (AR1)
  formula8 <- update(formula1,
    . ~ . + f(area_idx, model = "iid", group = cat_idx, control.group = list(model = "iid"),
              constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01)) +
            f(sur_idx, model = "ar1", group = cat_idx, control.group = list(model = "iid"),
              constr = TRUE, hyper = ar1_group_prior)
  )

  #' Model 9: category random effects (IID), age x category random effects (IID),
  #' space x category random effects (Besag), survey x category random effects (AR1)
  formula9 <- update(formula1,
    . ~ . + f(area_idx, model = "besag", graph = adjM, scale.model = TRUE, group = cat_idx,
              control.group = list(model = "iid"), constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01)) +
            f(sur_idx, model = "ar1", group = cat_idx, control.group = list(model = "iid"),
              constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01))
  )

  formulas <- append(formulas, parse(text = paste0("list(", paste0("formula", 4:9, collapse = ", "), ")")) %>% eval())
  models <- append(models, paste0("Model ", 4:9) %>% as.list())
}

#' Should the interaction models under development should be fit too?
if(include_interactions & include_temporal) {

  #' Model 5x:  category random effects (IID), age x category random effects (IID),
  #' space x category random effects (IID), survey x category random effects (IID),
  #' space x survey x category random effects (IID)
  formula5x <- update(formula5,
                      . ~ . + f(area_sur_idx, model = "iid", group = cat_idx, control.group = list(model = "iid"),
                                constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01))
  )

  #' Create Besag x IID interaction adjacency matrix
  #' Check the resulting matrix with image()
  interaction_adjM_6x <- multi.utils::repeat_matrix(adjM, n = length(unique(df$sur_idx)))

  #' Model 6x: category random effects (IID), age x category random effects (IID),
  #' space x category random effects (Besag), survey x category random effects (IID)
  #' space x survey x category random effects (Besag x IID)
  formula6x <- update(formula6,
                      . ~ . + f(area_sur_idx, model = "besag", graph = interaction_adjM_6x, scale.model = TRUE, group = cat_idx,
                                control.group = list(model = "iid"), constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01))
  )

  #' Model 8x:  category random effects (IID), age x category random effects (IID),
  #' space x category random effects (IID), survey x category random effects (AR1),
  #' space x survey x category random effects (IID x AR1)
  formula8x <- update(formula8,
                      . ~ . + f(area_idx_copy, model = "iid", group = sur_idx, replicate = cat_idx,
                                control.group = list(model = "ar1", hyper = list(rho = list(prior = "pc.cor1", param = c(0, 0.75)))),
                                constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01))
  )

  #' Model 9x: category random effects (IID), age x category random effects (IID),
  #' space x category random effects (Besag), survey x category random effects (AR1)
  #' space x survey x category random effects (Besag x AR1)
  formula9x <- update(
    formula9,
    . ~ . + f(area_idx_copy, model = "besag", graph = adjM, scale.model = TRUE, group = sur_idx, replicate = cat_idx,
              control.group = list(model = "ar1", hyper = list(rho = list(prior = "pc.cor1", param = c(0, 0.75)))),
              constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01))
  )

  formulas <- append(formulas, list(formula5x, formula6x, formula8x, formula9x))
  models <- append(models, list("Model 5x", "Model 6x", "Model 8x", "Model 9x"))
}

#' Fit the models

#' Number of Monte Carlo samples
S <- 100

res <- purrr::pmap(
  list(formula = formulas, model_name = models, S = S),
  multinomial_model
)

#' Extract the df and the full fitted models
res_df <- lapply(res, "[[", 1) %>% bind_rows()
res_fit <- lapply(res, "[[", 2)

#' Add columns for local DIC, WAIC, CPO
#' res_df has the 15-24 category too
res_df <- bind_cols(
  res_df,
  lapply(res_fit,
         function(fit) {
           return(data.frame(
             local_dic = fit$dic$local.dic,
             local_waic = fit$waic$local.waic,
             local_cpo = fit$cpo$cpo
           ))
         }
  ) %>%
    bind_rows() %>%
    #' Being safe here and explictly adding the NA entires for df_agg
    bind_rows(data.frame(
      local_dic = rep(NA, length(models) * nrow(df_agg)),
      local_waic = rep(NA, length(models) * nrow(df_agg)),
      local_cpo = rep(NA, length(models) * nrow(df_agg))
    ))
)

#' Artefact: Model selection information criteria for multinomial models
#' Some of the entries might be NA where there is missing data (INLA ignores these in its calculations)
ic_df <- sapply(res_fit, function(fit) {
  local_dic <- na.omit(fit$dic$local.dic)
  local_waic <- na.omit(fit$waic$local.waic)
  local_cpo <- na.omit(fit$cpo$cpo)

  c("dic" = sum(local_dic),
    "dic_se" = stats::sd(local_dic) * sqrt(length(local_dic)),
    "waic" = sum(local_waic),
    "waic_se" = stats::sd(local_waic) * sqrt(length(local_waic)),
    "cpo" = sum(local_cpo),
    "cpo_se" = stats::sd(local_cpo) * sqrt(length(local_cpo)))
}) %>%
  t() %>%
  round() %>%
  as.data.frame() %>%
  mutate(
    iso3 = iso3,
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
    iso3 = iso3,
    model = unlist(models),
    .before = everything()
  )

write_csv(variance_df, "variance-proportions.csv", na = "")

#' Artefact: Sample size recovery diagnostic
pdf("sample-size-recovery.pdf", h = 11.75, w = 8.25)

res_df %>%
  filter(age_group %in% c("Y015_019", "Y020_024", "Y025_029")) %>%
  group_by(obs_idx, model, survey_id) %>%
  summarise(
    n_eff_kish = mean(n_eff_kish),
    n_modelled_median = sum(lambda_median),
    n_modelled_lower = sum(lambda_lower),
    n_modelled_upper = sum(lambda_upper)
  ) %>%
  #' TODO: Add warning for this? Why are a few so high?
  filter(n_modelled_median < 1000) %>%
  mutate(n_modelled_upper_capped = pmin(n_modelled_upper, n_modelled_median + 100)) %>%
  ggplot(aes(
    x = n_eff_kish,
    y = n_modelled_median,
    ymin = n_modelled_lower,
    ymax = n_modelled_upper_capped,
  )) +
  geom_pointrange(alpha = 0.3) +
  facet_grid(survey_id ~ model) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", col = "#802D5B") +
  labs(x = "Kish ESS", y = "Sum of Poisson intensities",
       title = paste0(res_df$survey_id[1], ": are the sample sizes accurately recovered?"),
       subtitle = "Dashed line is x = y. Upper limit is cut off at 100 greater than median")

dev.off()

#' Artefact: Smoothed district indicator estimates for multinomial models
res_df <- res_df %>%
  #' Remove superfluous INLA indicator columns
  select(-area_idx, -cat_idx, -age_idx, -obs_idx, -age_cat_idx,
         -area_cat_idx, -sur_idx, -sur_cat_idx) %>%
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

#' Create plotting data
res_plot <- res_df %>%
  filter(area_id != iso3) %>%
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

pdf("multinomial-smoothed-district-sexbehav.pdf", h = 8.25, w = 11.75)

res_plot %>%
  split(~indicator + model) %>%
  lapply(function(x)
  x %>%
    mutate(
      age_group = fct_relevel(age_group, "Y015_024") %>%
        fct_recode(
          "15-19" = "Y015_019",
          "20-24" = "Y020_024",
          "25-29" = "Y025_029",
          "15-24" = "Y015_024"
        ),
      source = fct_relevel(source, "raw", "smoothed", "aggregate") %>%
        fct_recode("Survey raw" = "raw", "Smoothed" = "smoothed", "Admin 1 aggregate" = "aggr")
    ) %>%
    ggplot(aes(fill = estimate)) +
    geom_sf(size = 0.1) +
    scale_fill_viridis_c(option = "C", label = label_percent()) +
    facet_grid(age_group ~ survey_id + source) +
    theme_minimal() +
    labs(
      title = paste0(substr(x$survey_id[1], 1, 3), ": ", x$indicator[1], " (", x$model[1], ")")
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
)

dev.off()

#' Artefact: Stacked proportion barplots

res_df <- res_df %>%
  mutate(
    age_group = fct_relevel(age_group, "Y015_024", after = 3) %>%
      fct_recode(
        "15-19" = "Y015_019",
        "20-24" = "Y020_024",
        "25-29" = "Y025_029",
        "15-24" = "Y015_024"
      ),
    indicator = fct_recode(indicator,
      "No sex" = "nosex12m",
      "Cohabiting partner" = "sexcohab",
      "Nonregular partner" = "sexnonregplus"
    )
  )

pdf("stacked-proportions.pdf", h = 8.25, w = 11.75)

cbpalette <- c("#56B4E9", "#009E73", "#E69F00", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")

res_df %>%
  filter(area_id != iso3) %>%
  mutate(
    model = fct_recode(model,
      "1" = "Model 1", "2" = "Model 2", "3" = "Model 3",
      "4" = "Model 4", "5" = "Model 5", "6" = "Model 6",
      "7" = "Model 7", "8" = "Model 8", "9" = "Model 9"
    )
  ) %>%
  split(.$survey_id) %>%
  lapply(function(x) {
    ggplot(x, aes(x = model, y = estimate_smoothed, group = model, fill = indicator)) +
      geom_bar(position = "fill", stat = "identity", alpha = 0.8) +
      facet_grid(age_group ~ area_name, space = "free_x", scales = "free_x", switch = "x") +
      labs(x = "District", y = "Proportion", fill = "Category") +
      scale_color_manual(values = cbpalette) +
      theme_minimal() +
      labs(title = paste0(paste(unique(x$survey_id), collapse = ", "), ": posterior category mean proportions by model")) +
      theme(
        axis.text.x = element_blank(),
        plot.title = element_text(face = "bold"),
        legend.position = "bottom",
        legend.key.width = unit(4, "lines"),
        strip.placement = "outside",
        strip.text.x = element_text(angle = 90, hjust = 0)
      )
    }
  )

dev.off()

#' Artefact: Posterior predictive checks of coverage

pdf("coverage-histograms.pdf", h = 8.25, w = 11.75)

bins <- 20
alpha <- 0.05

ci <- qbinom(
  p = c(alpha / 2, 0.5, (1 - alpha / 2)),
  size = S,
  prob = 1 / bins
)

polygon_data <- data.frame(
  x = c(-0.05, 0, 1, 0, -0.05, 1.05, 1, 1.05, -0.05),
  y = c(ci[1], ci[2], ci[2], ci[2], ci[3], ci[3], ci[2], ci[1], ci[1]) / S
)

res_df %>%
  filter(
    area_id != iso3,
    age_group != "15-24"
  ) %>%
  split(.$model) %>%
  lapply(function(x) {
  ggplot(x, aes(x = prob_quantile)) +
    facet_grid(indicator ~ survey_id, drop = TRUE, scales = "free") +
    geom_histogram(aes(y = (..count..) / tapply(..count..,..PANEL..,sum)[..PANEL..]),
                   breaks = seq(0, 1, length.out = bins + 1), fill = "#009E73", col = "black", alpha = 0.9) +
    geom_polygon(data = polygon_data, aes(x = x, y = y), fill = "grey75", color = "grey50", alpha = 0.6) +
    labs(title = paste0(x$model[1]), x = "", y = "") +
    scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c(0, 0.25, 0.5, 0.75, 1))
  })

dev.off()

pdf("coverage-ecdf-diff.pdf", h = 10, w = 12)

lims <- get_lims(n = S, alpha, K = 100)

res_df %>%
  split(.$model) %>%
  lapply(function(x) {
  x %>%
    select(indicator, survey_id, prob_quantile) %>%
    filter(!is.na(prob_quantile)) %>%
    split(~ indicator + survey_id) %>%
    lapply(function(y) {
      empirical_coverage <- purrr::map_dbl(seq(0, 1, by = 0.01), ~ empirical_coverage(y$prob_quantile, .x))
      data.frame(nominal_coverage = seq(0, 1, by = 0.01), empirical_coverage = empirical_coverage) %>%
        mutate(
          ecdf_diff = empirical_coverage - nominal_coverage,
          ecdf_diff_lower = lims$lower / S - nominal_coverage,
          ecdf_diff_upper = lims$upper / S - nominal_coverage,
        )
    }) %>%
    purrr::map_df(~as.data.frame(.x), .id = c("indicator.survey_id")) %>%
    separate(indicator.survey_id, c("indicator", "survey_id"), sep = "[.]") %>%
    ggplot(aes(x = nominal_coverage, y = ecdf_diff)) +
      facet_grid(indicator ~ survey_id, drop = TRUE, scales = "free") +
      geom_line(col = "#009E73") +
      geom_step(aes(x = nominal_coverage, y = ecdf_diff_upper), alpha = 0.7) +
      geom_step(aes(x = nominal_coverage, y = ecdf_diff_lower), alpha = 0.7) +
      geom_abline(intercept = 0, slope = 0, linetype = "dashed", col = "grey50") +
      labs(title = paste0(x$model[1]), x = "", y = "ECDF difference") +
      scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c(0, 0.25, 0.5, 0.75, 1))
  })

dev.off()

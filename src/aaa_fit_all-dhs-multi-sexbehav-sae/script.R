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

#' Set ind > 1 to 1, as well as ind < 0 to 0
message(
  paste0(
    "There are: ",
    filter(ind, estimate < 0) %>% nrow(), " values of ind$estimate < 0 and ",
    filter(ind, estimate > 1) %>% nrow(), " values of ind$estimate > 1.
    If they exist, these values have been set to be inside [0, 1]!"
  )
)

ind <- ind %>%
  mutate(estimate = pmin(1, estimate),
         estimate = pmax(0, estimate))

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

#' Create the scaffolding for the estimates
df <- crossing(
  #' In this model we are using three risk categories (rather than four)
  indicator = c("nosex12m", "sexcohab", "sexnonregplus"),
  #' Add in the survey_id to deal with multiple surveys
  survey_id = unique(ind$survey_id),
  #' Just these three age groups, aim to calculate aggregates at later point
  age_group = c("Y015_019", "Y020_024", "Y025_029"),
  areas_model %>%
    st_drop_geometry() %>%
    select(area_id, area_name, area_idx, area_id_aggr,
           area_sort_order, center_x, center_y)
)

#' Add district observations to the scaffolding by merging with ind
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
         area_cat_idx = interaction(area_idx, cat_idx),
         #' Is the best way to add obs_idx? Perhaps can be added earlier in the pipeline
         obs_idx = to_int(interaction(age_idx, area_idx, sur_idx))) %>%
  arrange(obs_idx)

#' Specify the models to be fit

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
#' TODO: Might it be the case that we want space x time x category random effects? Can this be handled?

#' Model 1: category random effects (IID), age x category random effects (IID)
formula1 <- x_eff ~ -1 + f(obs_idx, model = "iid", hyper = tau_prior(0.000001)) +
  f(cat_idx, model = "iid", constr = TRUE, hyper = tau_prior(0.001)) +
  f(age_cat_idx, model = "iid", constr = TRUE, hyper = tau_prior(0.001))

#' Model 2: category random effects (IID), age x category random effects (IID),
#' space x category random effects (IID)
formula2 <- x_eff ~ -1 + f(obs_idx, model = "iid", hyper = tau_prior(0.000001)) +
  f(cat_idx, model = "iid", constr = TRUE, hyper = tau_prior(0.001)) +
  f(age_cat_idx, model = "iid", constr = TRUE, hyper = tau_prior(0.001)) +
  f(area_idx, model = "iid", group = cat_idx, control.group = list(model = "iid"),
    constr = TRUE, hyper = tau_prior(0.001))

#' Model 3: category random effects (IID), age x category random effects (IID),
#' space x category random effects (BYM2)
formula3 <- x_eff ~ -1 + f(obs_idx, model = "iid", hyper = tau_prior(0.000001)) +
  f(cat_idx, model = "iid", constr = TRUE, hyper = tau_prior(0.001)) +
  f(age_cat_idx, model = "iid", constr = TRUE, hyper = tau_prior(0.001)) +
  f(area_idx, model = "besag", graph = adjM, scale.model = TRUE, group = cat_idx,
    control.group = list(model = "iid"), constr = TRUE, hyper = tau_prior(0.001))

#' Model 4:  category random effects (IID), age x category random effects (IID),
#' survey x category random effects (IID) (grouped)
formula4 <- x_eff ~ -1 + f(obs_idx, model = "iid", hyper = tau_prior(0.000001)) +
  f(cat_idx, model = "iid", constr = TRUE, hyper = tau_prior(0.001)) +
  f(age_cat_idx, model = "iid", constr = TRUE, hyper = tau_prior(0.001)) +
  f(sur_idx, model = "iid", group = cat_idx, control.group = list(model = "iid"),
    constr = TRUE, hyper = tau_prior(0.001))

#' Model 5:  category random effects (IID), age x category random effects (IID),
#' space x category random effects (IID), survey x category random effects (IID) (grouped)
formula5 <- x_eff ~ -1 + f(obs_idx, model = "iid", hyper = tau_prior(0.000001)) +
  f(cat_idx, model = "iid", constr = TRUE, hyper = tau_prior(0.001)) +
  f(age_cat_idx, model = "iid", constr = TRUE, hyper = tau_prior(0.001)) +
  f(area_idx, model = "iid", group = cat_idx, control.group = list(model = "iid"),
    constr = TRUE, hyper = tau_prior(0.001)) +
  f(sur_idx, model = "iid", group = cat_idx, control.group = list(model = "iid"),
    constr = TRUE, hyper = tau_prior(0.001))

#' Model 6: category random effects (IID), age x category random effects (IID),
#' space x category random effects (BYM2), survey x category random effects (IID) (grouped)
formula6 <- x_eff ~ -1 + f(obs_idx, model = "iid", hyper = tau_prior(0.000001)) +
  f(cat_idx, model = "iid", constr = TRUE, hyper = tau_prior(0.001)) +
  f(age_cat_idx, model = "iid", constr = TRUE, hyper = tau_prior(0.001)) +
  f(area_idx, model = "besag", graph = adjM, scale.model = TRUE, group = cat_idx,
    control.group = list(model = "iid"), constr = TRUE, hyper = tau_prior(0.001)) +
  f(sur_idx, model = "iid", group = cat_idx, control.group = list(model = "iid"),
    constr = TRUE, hyper = tau_prior(0.001))

#' Model 7:  category random effects (IID), age x category random effects (IID),
#' survey x category random effects (AR1)
formula7 <- x_eff ~ -1 + f(obs_idx, model = "iid", hyper = tau_prior(0.000001)) +
  f(cat_idx, model = "iid", constr = TRUE, hyper = tau_prior(0.001)) +
  f(age_cat_idx, model = "iid", constr = TRUE, hyper = tau_prior(0.001))+
  f(sur_idx, model = "ar1", group = cat_idx, control.group = list(model = "iid"),
    constr = TRUE, hyper = tau_prior(0.001))

#' Model 8:  category random effects (IID), age x category random effects (IID),
#' space x category random effects (IID), survey x category random effects (AR1)
formula8 <- x_eff ~ -1 + f(obs_idx, model = "iid", hyper = tau_prior(0.000001)) +
  f(cat_idx, model = "iid", constr = TRUE, hyper = tau_prior(0.001)) +
  f(age_cat_idx, model = "iid", constr = TRUE, hyper = tau_prior(0.001)) +
  f(area_idx, model = "iid", group = cat_idx, control.group = list(model = "iid"),
    constr = TRUE, hyper = tau_prior(0.001)) +
  f(sur_idx, model = "ar1", group = cat_idx, control.group = list(model = "iid"),
    constr = TRUE, hyper = tau_prior(0.001))

#' Model 9: category random effects (IID), age x category random effects (IID),
#' space x category random effects (BYM2), survey x category random effects (AR1)
formula9 <- x_eff ~ -1 + f(obs_idx, model = "iid", hyper = tau_prior(0.000001)) +
  f(cat_idx, model = "iid", constr = TRUE, hyper = tau_prior(0.001)) +
  f(age_cat_idx, model = "iid", constr = TRUE, hyper = tau_prior(0.001)) +
  f(area_idx, model = "besag", graph = adjM, scale.model = TRUE, group = cat_idx,
    control.group = list(model = "iid"), constr = TRUE, hyper = tau_prior(0.001)) +
  f(sur_idx, model = "ar1", group = cat_idx, control.group = list(model = "iid"),
    constr = TRUE, hyper = tau_prior(0.001))

#' All of the possible models
all_formulas <- parse(text = paste0("list(", paste0("formula", 1:9, collapse = ", "), ")")) %>% eval()
all_models <- paste0("Model ", 1:9) %>% as.list()

#' The subset of all possible fit in this script, as specified by model_ids
formulas <- all_formulas[1:max_model_id]
models <- all_models[1:max_model_id]

#' Fit the models
res <- purrr::pmap(
  list(formula = formulas, model_name = models),
  multinomial_model
)

#' Extract the df and the full fitted models
res_df <- lapply(res, "[[", 1) %>% bind_rows()
res_fit <- lapply(res, "[[", 2)

#' Add columns for local DIC and WAIC
res_df <- bind_cols(
  res_df,
  lapply(res_fit,
         function(fit) {
           return(data.frame(
             local_dic = fit$dic$local.dic,
             local_waic = fit$waic$local.waic
           ))
         }
  ) %>% bind_rows()
)

#' Prepare data for writing to output
res_df <- res_df %>%
  #' Remove superfluous INLA indicator columns
  select(-area_idx, -cat_idx, -age_idx, -obs_idx, -age_cat_idx,
         -area_cat_idx, -sur_idx, -sur_cat_idx) %>%
  #' Make it clear which of the estimates are raw and which are from the model (smoothed)
  rename(
    estimate_raw = estimate,
    ci_lower_raw = ci_lower,
    ci_upper_raw = ci_upper,
    estimate_smoothed = mean,
    median_smoothed = median,
    ci_lower_smoothed = lower,
    ci_upper_smoothed = upper
  ) %>%
  mutate(iso3 = iso3, .before = indicator) %>%
  relocate(model, .before = estimate_smoothed)

write_csv(res_df, "multinomial-smoothed-district-sexbehav.csv", na = "")

#' Simple model comparison for output
#' Some of the local DIC or local WAIC entries might be NA where there is no raw data
#' INLA calculates the DIC or WAIC ignoring these
ic_df <- sapply(res_fit, function(fit) {
  local_dic <- na.omit(fit$dic$local.dic)
  local_waic <- na.omit(fit$waic$local.waic)

  c("dic" = sum(local_dic),
    "dic_se" = stats::sd(local_dic) * sqrt(length(local_dic)),
    "waic" = sum(local_waic),
    "waic_se" = stats::sd(local_waic) * sqrt(length(local_waic)))
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

#' Create plotting data
res_plot <- res_df %>%
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
  split(~indicator + model)

#' Cloropleths

pdf("multinomial-smoothed-district-sexbehav.pdf", h = 11, w = 8.5)

lapply(res_plot, function(x)
  x %>%
    mutate(
      age_group = fct_relevel(age_group, "Y015_024") %>%
        fct_recode("15-19" = "Y015_019", "20-24" = "Y020_024", "25-29" = "Y025_029", ),
      source = fct_relevel(source, "raw", "smoothed", "aggregate") %>%
        fct_recode("Survey raw" = "raw", "Smoothed" = "smoothed", "Admin 1 aggregate" = "aggr")
    ) %>%
    ggplot(aes(fill = estimate)) +
    geom_sf(size = 0.1) +
    scale_fill_viridis_c(option = "C", label = label_percent()) +
    facet_grid(age_group ~ survey_id + source) +
    theme_minimal() +
    labs(
      title = paste0(paste(unique(x$survey_id), collapse = ", "), ": ", x$indicator[1], " (", x$model[1], ")")
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

#' Stacked proportion plots

pdf("stacked-proportions.pdf", h = 10, w = 12)

cbpalette <- c("#56B4E9","#009E73", "#E69F00", "#F0E442","#0072B2","#D55E00","#CC79A7", "#999999")

res_df %>% split(.$survey_id) %>% lapply(function(x)
  x %>% mutate(
    age_group = fct_relevel(age_group, "Y015_024", after = 3) %>%
      fct_recode(
        "15-19" = "Y015_019",
        "20-24" = "Y020_024",
        "25-29" = "Y025_029",
        "15-24" = "Y015_024"
      ),
    model = fct_recode(model,
                       "1" = "Model 1",
                       "2" = "Model 2",
                       "3" = "Model 3",
                       "4" = "Model 4",
                       "5" = "Model 5",
                       "6" = "Model 6",
                       "7" = "Model 7",
                       "8" = "Model 8",
                       "9" = "Model 9"
    ),
    indicator = fct_recode(indicator,
                           "No sex (past 12 months)" = "nosex12m",
                           "Cohabiting partner" = "sexcohab",
                           "Nonregular partner(s)" = "sexnonreg",
                           "Paid for sex (past 12 months)" = "sexpaid12m"
    )
  ) %>%
  ggplot(aes(x = model, y = estimate_smoothed, group = model, fill = indicator)) +
  geom_bar(position = "fill", stat = "identity", alpha = 0.8) +
  facet_grid(age_group ~ area_name, space = "free_x", scales = "free_x", switch = "x") +
  labs(x = "District", y = "Proportion", fill = "Category") +
  scale_color_manual(values = cbpalette) +
  theme_minimal() +
  labs(title = paste0(x$survey_id[1], ": posterior category mean proportions by model")) +
  theme(
    axis.text.x = element_blank(),
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.key.width = unit(4, "lines"),
    strip.placement = "outside",
    strip.text.x = element_text(angle = 90, hjust = 0)
  )
)

dev.off()

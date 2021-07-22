#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("aaa_fit_multi-sexbehav-sae", parameters = list(iso3 = "MWI", max_model_id = 3))
# setwd("src/aaa_fit_multi-sexbehav-sae")

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

#' In this report, we only use the most recent DHS survey from each country
#' max(ind$survey_id) is plausibly dodgy: survey_id is a string
ind <- ind %>%
  filter(survey_id == max(ind$survey_id))

#' Append an indicator for 1 - sex12m
#' This earlier in the pipeline with a mutate call, e.g. modify naomi.utils
#' Though it's probably not necessary and this works OK seeing as it's not so
#' difficult to calculate the standard errors for 1 - existing_indicator
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
#' TODO: Investigate the data issues leading to this by clicking through this script, changing the iso3
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
  #' Using nosex12m rather than e.g. nosex
  indicator = c("nosex12m", "sexcohab", "sexnonreg", "sexpaid12m"),
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
    by = c("indicator", "age_group", "area_id")
  )

#' Add indicies for:
#'  * age (age_idx)
#'  * category (cat_idx)
#'  * observation (obs_idx)
#'  * age x category (age_cat_idx)
#'  * space x category (area_idx)
df <- df %>%
  mutate(cat_idx = to_int(indicator),
         age_idx = to_int(age_group),
         age_cat_idx = interaction(age_idx, cat_idx),
         #' Is the best way to do it for obs_idx? Perhaps can be added earlier in the pipeline
         obs_idx = to_int(interaction(age_idx, area_idx)),
         #' Likewise probably a better way to do this (using the INLA group option)
         area_idx.1 = ifelse(cat_idx == 1, area_idx, NA),
         area_idx.2 = ifelse(cat_idx == 2, area_idx, NA),
         area_idx.3 = ifelse(cat_idx == 3, area_idx, NA),
         area_idx.4 = ifelse(cat_idx == 4, area_idx, NA)) %>%
  arrange(obs_idx)

#' Add population data


#' Specify the models to be fit

#' Model 1: age x category random effects (IID)
formula1 <- x_eff ~ -1 + f(obs_idx, hyper = tau_prior(0.000001)) +
  f(age_cat_idx, model = "iid", constr = TRUE, hyper = tau_prior(0.001))

#' Model 2: space x category random effects (IID)
formula2 <- x_eff ~ -1 + f(obs_idx, hyper = tau_prior(0.000001)) +
  f(age_cat_idx, model = "iid", constr = TRUE, hyper = tau_prior(0.001)) +
  f(area_idx.1, model = "iid", constr = TRUE, hyper = tau_prior(0.001)) +
  f(area_idx.2, model = "iid", constr = TRUE, hyper = tau_prior(0.001)) +
  f(area_idx.3, model = "iid", constr = TRUE, hyper = tau_prior(0.001)) +
  f(area_idx.4, model = "iid", constr = TRUE, hyper = tau_prior(0.001))

#' Model 3: space x category random effects (BYM2)
formula3 <- x_eff ~ -1 + f(obs_idx, hyper = tau_prior(0.000001)) +
  f(age_cat_idx, model = "iid", constr = TRUE, hyper = tau_prior(0.001)) +
  f(area_idx.1, model = "bym2", graph = adjM, constr = TRUE, hyper = tau_prior(0.001)) +
  f(area_idx.2, model = "bym2", graph = adjM, constr = TRUE, hyper = tau_prior(0.001)) +
  f(area_idx.3, model = "bym2", graph = adjM, constr = TRUE, hyper = tau_prior(0.001)) +
  f(area_idx.4, model = "bym2", graph = adjM, constr = TRUE, hyper = tau_prior(0.001))

#' Model 4: As Model 2, but using the group option
#' TODO: Better understand this from
#' https://raw.githubusercontent.com/hrue/r-inla/devel/internal-doc/group/group-models.pdf
#' "There are much more applications, e.g. invariant smoothing of multinomial data"
formula4 <- x_eff ~ -1 + f(obs_idx, hyper = tau_prior(0.000001)) +
  f(age_cat_idx, model = "iid", constr = TRUE, hyper = tau_prior(0.001)) +
  f(area_idx, model = "iid", group = cat_idx,
    control.group = list(model = "iid"), constr = TRUE, hyper = tau_prior(0.001))

#' Model 5: As Model 3, but using the group option
formula5 <- x_eff ~ -1 + f(obs_idx, hyper = tau_prior(0.000001)) +
  f(age_cat_idx, model = "iid", constr = TRUE, hyper = tau_prior(0.001)) +
  f(area_idx, model = "bym2", graph = adjM, group = cat_idx,
    control.group = list(model = "iid"), constr = TRUE, hyper = tau_prior(0.001))

#' All of the possible models
all_formulas <- parse(text = paste0("list(", paste0("formula", 1:5, collapse = ", "), ")")) %>% eval()
all_models <- list("Model 1: Constant", "Model 2: IID", "Model 3: BYM2", "Model 4: IID (grouped)", "Model 5: BYM2 (grouped)")

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
         -area_idx.1, -area_idx.2, -area_idx.3, -area_idx.4) %>%
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

#' Diagnostics for the local information criteria causing issues
res_df <- res_df %>%
  mutate(large_local_dic = ifelse(log10(abs(local_dic)) > 5, TRUE, FALSE),
         large_local_waic = ifelse(log10(abs(local_waic)) > 5, TRUE, FALSE))

mean_x_eff_for_large_local_dic <- res_df %>%
  filter(large_local_dic == TRUE) %>%
  summarise(mean = mean(x_eff)) %>%
  as.numeric()

mean_x_eff_for_large_local_waic <- res_df %>%
  filter(large_local_waic == TRUE) %>%
  summarise(mean = mean(x_eff)) %>%
  as.numeric()

pdf("local-ic-diagnostics.pdf", h = 11, w = 8.5)

#' Highlighting the problem with having x_eff = 0, particularly in sexpaid12m
ggplot(res_df, aes(x = 1:nrow(res_df), y = log10(abs(local_dic)), col = large_local_dic)) +
    geom_point(alpha = 0.4) +
    facet_grid(indicator ~ model) +
    labs(x = "Index", y = "log10|DIC|", col = "Large local DIC",
         title = paste0(res_df$survey_id[1], ": are any of the observations causing problems for DIC?"),
         subtitle = ifelse(
           is.nan(mean_x_eff_for_large_local_dic),
           "There are no observations with large local DIC",
           paste0("The average value of x_eff for observations with large local DIC is ", mean_x_eff_for_large_local_dic)
         )) +
    scale_color_manual(values = c("black", "#802D5B")) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggplot(res_df, aes(x = 1:nrow(res_df), y = log10(abs(local_waic)), col = large_local_waic)) +
  geom_point(alpha = 0.4) +
  facet_grid(indicator ~ model) +
  labs(x = "Index", y = "log10|WAIC|", col = "Large local WAIC",
       title = paste0(res_df$survey_id[1], ": are any of the observations causing problems for WAIC?"),
       subtitle = ifelse(
         is.nan(mean_x_eff_for_large_local_waic),
         "There are no observations with large local WAIC",
         paste0("The average value of x_eff for observations with large local WAIC is ", mean_x_eff_for_large_local_waic)
       )) +
  scale_color_manual(values = c("black", "#802D5B")) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

dev.off()

#' Simple model comparison data for output
#' Something strange happening with WAIC here, unreasonable orders of magnitude
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

pdf("multinomial-smoothed-district-sexbehav.pdf", h = 11, w = 8.5)

lapply(res_plot, function(x)
  x %>%
    mutate(
      age_group = fct_relevel(age_group, "Y015_024") %>%
        fct_recode("15-19" = "Y015_019", "20-24" = "Y020_024", "25-29" = "Y025_029"),
      #' Note: not currently using aggregate, right?
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

pdf("bym2-proportions.pdf", h = 11, w = 8.5)

if(max_model_id >= 3) {

#' Check the mixing parameters in the BYM2 model
lapply(1:4, function(i) {
  bym2_fit <- res_fit[[3]]
  mean <- bym2_fit$summary.hyperpar[i, 1] %>% round(digits = 3)
  sd <- bym2_fit$summary.hyperpar[i, 2] %>% round(digits = 3)
  bym2_fit$marginals.hyperpar[[i]] %>%
    as.data.frame() %>%
    ggplot(aes(x = x, y = y)) +
    geom_line() +
    labs(title = paste0(res_df$survey_id[1], ": posterior of the BYM2 proportion parameter in category ", i),
         subtitle = paste0("Mean: ", mean, ", SD: ", sd),
         x = "Proportion", y = "p(Proportion)") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"))
  })

}

dev.off()

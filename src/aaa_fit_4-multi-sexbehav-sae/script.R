#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("aaa_fit_4-multi-sexbehav-sae", parameters = list(iso3 = "MWI"))
# setwd("src/aaa_fit_4-multi-sexbehav-sae")

analysis_level <- multi.utils::analysis_level()
admin1_level <- multi.utils::admin1_level()

stopifnot(iso3 %in% names(analysis_level))
stopifnot(iso3 %in% names(admin1_level))

analysis_level <- analysis_level[iso3]
admin1_level <- admin1_level[iso3]

areas <- read_sf(paste0("depends/", tolower(iso3), "_areas.geojson"))
ind <- read_csv(paste0("depends/", tolower(iso3), "_survey_indicators_sexbehav.csv"))
pop <- read_csv("depends/interpolated-population.csv")

#' Use only the surveys which contain a specific paid sex question
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
  pull(survey_id)

ind <- ind %>%
  filter(survey_id %in% giftsvar_surveys)

#' Set ind$estimate > 1 to 1, as well as ind$estimate < 0 to 0
ind$estimate <- constrain_interval(ind$estimate, lower = 0, upper = 1)

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
  #' In this model we use all of the four risk categories
  #' Use nosex12m rather than e.g. nosex
  indicator = c("nosex12m", "sexcohab", "sexnonreg", "sexpaid12m"),
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
    by = c("indicator", "age_group", "area_id")
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
#'  * age (age_idx)
#'  * category (cat_idx)
#'  * observation (obs_idx)
#'  * age x category (age_cat_idx)
#'  * space x category (area_cat_idx)
df <- df %>%
  mutate(
    cat_idx = to_int(indicator),
    #' Doing this because want Y015_024 to have ID 4 rather than 2 as it would be otherwise
    age_idx = as.integer(factor(age_group, levels = c("Y015_019", "Y020_024", "Y025_029", "Y015_024"))),
    age_cat_idx = to_int(interaction(age_idx, cat_idx)),
    area_cat_idx = to_int(interaction(area_idx, cat_idx)),
    obs_idx = to_int(interaction(age_idx, area_idx))
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

#' Model 1: category random effects (IID), age x category random effects (IID)
formula1 <- x_eff ~ -1 + f(obs_idx, model = "iid", hyper = tau_fixed(0.000001)) +
  f(cat_idx, model = "iid", constr = TRUE, hyper = tau_pc(x = 0.001, u = 2.5, alpha = 0.01)) +
  f(age_idx, model = "iid", group = cat_idx, control.group = list(model = "iid"),
    constr = TRUE, hyper = tau_pc(x = 0.001, u = 2.5, alpha = 0.01))

#' Model 2: category random effects (IID), age x category random effects (IID),
#' space x category random effects (IID)
formula2 <- update(formula1,
                   . ~ . + f(area_idx, model = "iid", group = cat_idx, control.group = list(model = "iid"),
                             constr = TRUE, hyper = tau_pc(x = 0.001, u = 2.5, alpha = 0.01))
)

#' Model 3: category random effects (IID), age x category random effects (IID),
#' space x category random effects (Besag)
formula3 <- update(formula1,
                   . ~ . + f(area_idx, model = "besag", graph = adjM, scale.model = TRUE, group = cat_idx,
                             control.group = list(model = "iid"), constr = TRUE, hyper = tau_pc(x = 0.001, u = 2.5, alpha = 0.01))
)

#' All of the possible models
#' parse(text = paste0("list(", paste0("formula", 1:3, collapse = ", "), ")")) %>% eval()
formulas <- list(formula1, formula2, formula3)
models <- list("Model 1", "Model 2", "Model 3")

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
ic <- lapply(res_fit, function(fit) {
  data.frame(
    local_dic = fit$dic$local.dic,
    local_waic = fit$waic$local.waic,
    local_cpo = fit$cpo$cpo
  ) %>%
    bind_rows(
      setNames(as.data.frame(matrix(data = NA, nrow = nrow(df_agg), ncol = 3)), c("local_dic", "local_waic", "local_cpo"))
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
    model = unlist(models),,
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
       title = paste0(substr(res_df$survey_id[1], 1, 3), ": are the sample sizes accurately recovered?"),
       subtitle = "Dashed line is x = y. Upper limit is cut off at 100 greater than median") +
  theme_minimal()

dev.off()

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
        source = fct_relevel(source, "raw", "smoothed") %>%
          fct_recode("Survey raw" = "raw", "Smoothed" = "smoothed")
      ) %>%
      ggplot(aes(fill = estimate)) +
      geom_sf(size = 0.1, colour = scales::alpha("grey", 0.25)) +
      scale_fill_viridis_c(option = "C", label = label_percent()) +
      facet_grid(age_group ~ survey_id + source) +
      theme_minimal() +
      labs(
        title = paste0(substr(x$survey_id, 1, 3), ": ", x$indicator[1], " (", x$model[1], ")")
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

pdf("stacked-proportions.pdf", h = 8.25, w = 11.75)

#' In this plot, it'd be great if there was some way to label that 1, 2, 3, ... refer to models,
#' though I think it could reasonably be done in the description for the figure.
#'
#' It might also be nice to include uncertainty, with some options discussed in this thread:
#' https://twitter.com/solomonkurz/status/1372632774285348864
#' However, I do not see that any of the options are good for this plot, and there is already a
#' lot of information being displayed without adding the uncertainty.

res_df %>%
  filter(area_id != iso3) %>%
  split(.$survey_id) %>% lapply(function(x)
    x %>% mutate(
      age_group = fct_relevel(age_group, "Y015_024", after = 3) %>%
        fct_recode(
          "15-19" = "Y015_019",
          "20-24" = "Y020_024",
          "25-29" = "Y025_029",
          "15-24" = "Y015_024"
        ),
      model = fct_recode(model, "1" = "Model 1", "2" = "Model 2", "3" = "Model 3"),
      indicator = fct_recode(indicator,
                             "No sex (past 12 months)" = "nosex12m",
                             "Cohabiting partner" = "sexcohab",
                             "Nonregular partner(s) or paid for sex (past 12 months)" = "sexnonreg",
                             "Female sex woker" = "sexpaid12m"
      )
    ) %>%
      ggplot(aes(x = model, y = estimate_smoothed, group = model, fill = indicator)) +
      geom_bar(position = "fill", stat = "identity", alpha = 0.8) +
      facet_grid(age_group ~ area_name, space = "free_x", scales = "free_x", switch = "x") +
      labs(x = "District", y = "Proportion", fill = "Category") +
      scale_color_manual(values = multi.utils::cbpalette()) +
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
  )

dev.off()

#' Diagnostics for the local information criteria causing issues
res_df <- res_df %>%
  mutate(
    large_local_dic = ifelse(log10(abs(local_dic)) > 5, TRUE, FALSE),
    large_local_waic = ifelse(log10(abs(local_waic)) > 5, TRUE, FALSE)
  )

mean_x_eff_for_large_local_dic <- res_df %>%
  filter(large_local_dic == TRUE) %>%
  summarise(mean = mean(x_eff)) %>%
  as.numeric()

mean_x_eff_for_large_local_waic <- res_df %>%
  filter(large_local_waic == TRUE) %>%
  summarise(mean = mean(x_eff)) %>%
  as.numeric()

pdf("local-ic-diagnostics.pdf", h = 11.75, w = 8.25)

#' Highlighting the problem with having x_eff = 0, particularly in sexpaid12m
#' Note that warnings here about removing rows with missing values are due to the aggregate age entries
#' not having local DIC or WAIC values
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

#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("fit_fsw-logit-sae")
# setwd("src/fit_fsw-logit-sae")

sf_use_s2(FALSE)

analysis_level <- multi.utils::analysis_level()
admin1_level <- multi.utils::admin1_level()
iso3 <- names(analysis_level)

areas <- readRDS("depends/areas.rds")
ind <- read_csv("depends/survey_indicators_sexbehav.csv")
cfsw_ever <- read_csv("everpaidforsex-bycountry.csv")
cfsw_recent <- read_csv("recentpaidforsex-bycountry.csv")

#' Set ind$estimate > 1 to 1, as well as ind$estimate < 0 to 0
ind$estimate <- constrain_interval(ind$estimate, lower = 0, upper = 1)

#' Use only the surveys which contain a specific paid sex question
available_surveys <- read_csv("depends/available-surveys.csv")

(giftsvar_surveys <- available_surveys %>%
  filter(giftsvar == 1) %>%
  pull(survey_id))

ind <- filter(ind, survey_id %in% giftsvar_surveys)

#' There are this many surveys which are suitable
length(unique(ind$survey_id))

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
  ) %>%
  #' Add a country name column (useful for merging covariate data)
  mutate(
    country = fct_recode(iso3,
      "Botswana" = "BWA",
      "Cameroon" = "CMR",
      "Kenya" = "KEN",
      "Lesotho" = "LSO",
      "Mozambique" = "MOZ",
      "Malawi" = "MWI",
      "Namibia" = "NAM",
      "Eswatini" = "SWZ",
      "Tanzania" = "TZA",
      "Uganda" = "UGA",
      "South Africa" = "ZAF",
      "Zambia" = "ZMB",
      "Zimbabwe" = "ZWE"
    )
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

#' Create the scaffolding for the estimates
df <- crossing(
  #' Only the high and very high risk groups (sexnonregplus includes both)
  indicator = c("sexnonreg", "sexpaid12m"),
  #' Three age groups, plus aggregate category
  age_group = c("Y015_019", "Y020_024", "Y025_029"),
  #' Both the areas in the model and the aggregate country
  areas_model %>%
    st_drop_geometry() %>%
    select(country, iso3, area_id, area_name, area_level,
           area_idx, area_sort_order, center_x, center_y)
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

#' Merge covariates into df
#' * cfswever: Proportion of men who have ever paid for sex (national level)
#' * csfwrecent: Proporiton of men who have recently (past 12 months) paid for sex (national level)
df <- df %>%
  left_join(
    cfsw_ever %>%
      select(country = Country, cfswever = EverPaidSex),
    by = "country"
  ) %>%
  left_join(
    cfsw_recent %>%
      select(country = Country, cfswrecent = PaidSexLast12Months),
    by = "country"
  )

#' Add fake survey IDs
df <- mutate(df, survey_id = replace_na(survey_id, "Missing"))

#' Add indicies for:
df <- mutate(df,
    #' age
    age_idx = multi.utils::to_int(age_group),
    #' survey
    sur_idx = multi.utils::to_int(survey_id),
    #' country
    iso3_idx = multi.utils::to_int(iso3),
    #' observation
    obs_idx = multi.utils::to_int(interaction(age_idx, area_idx, sur_idx)),
  )

#' The proportion of (sexnonreg + sexpaid12m) who are sexpaid12m should have:
#' * sample size x_eff from (sexnonreg + sexpaid12m),
#' * and observations x_eff from sexpaid12m
#' This is a slow way of doing this: perhaps after this project I should learn datatable or try dtplyr
df <- df %>%
  #' For each observation...
  split(.$obs_idx) %>%
  lapply(function(x)
    x %>%
      #' ... n_eff_kish is sum of x_eff from sexnonreg and sexpaid12m
      mutate(n_eff_kish = sum(x$x_eff)) %>%
      #' and we just want to keep the row for sexpaid12m (such that x_eff corresponds to sexpaid12m)
      filter(indicator == "sexpaid12m") %>%
      #' Rename to propsexpaid12m, update estimate, and remove columns that don't make sense anymore
      mutate(
        indicator = "propsexpaid12m",
        estimate = x_eff / n_eff_kish
      ) %>%
      #' How would you calculate the ci_lower and ci_upper? Can't just do the worst possible case in both
      #' e.g. ci_lower[sexpaid12m] / (ci_upper[sexpaid12m] + ci_upper[sexnonreg]) as this would be more than
      #' a 95% quantile outcome. Just going to delete them for now
      select(-ci_lower, -ci_upper)
  ) %>%
  bind_rows()

#' For the DHS surveys, specific sex paid question not asked to 25-29
#' Three-partner loop used instead, so set these to NA because not comparable
df <- df %>%
  mutate(
    condition = (substr(survey_id, 8, 11) == "DHS") & (age_group == "Y025_029"),
    x_eff = ifelse(condition, NA, x_eff),
    n_eff_kish = ifelse(condition, NA, n_eff_kish),
    estimate = ifelse(condition, NA, estimate)
  ) %>%
  select(-condition)

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
    facet_wrap(~age_group) +
    labs(title = paste0(x$survey_id[1])) +
    theme_minimal()
  })

dev.off()

#' Check for larger observations than sample size: this shouldn't trigger!
if(sum(df$n_eff_kish < df$x_eff, na.rm = TRUE) > 0) {
  message(
    paste0(
      "There are ", sum(df$n_eff_kish < df$x_eff), " rows when x_eff > n_eff_kish!",
      "\nThis occurs in the following surveys:\n",
      paste(unique(filter(df, n_eff_kish < x_eff)$survey_id), collapse = ", "),
      "\nx_eff and n_eff_kish have been set to zero in these cases."
    )
  )

  df <- mutate(df,
      x_eff = ifelse(n_eff_kish < x_eff, 0, x_eff),
      n_eff_kish = ifelse(n_eff_kish < x_eff, 0, n_eff_kish)
    )
}

#' x_eff = 0 and n_eff_kish = 0 is causing INLA to crash (could be investigated further)
#' Take the approach of setting both to NA in this case (no information either way)
df <- mutate(df,
    x_eff = ifelse(n_eff_kish == 0 & x_eff == 0, NA, x_eff),
    n_eff_kish = ifelse(n_eff_kish == 0 & x_eff == 0, NA, n_eff_kish)
  )

pdf("covariate-correlation-check.pdf", h = 5, w = 6.25)

ggplot(df, aes(x = cfswrecent)) +
  geom_histogram() +
  theme_minimal()

ggplot(df, aes(x = cfswever)) +
  geom_histogram() +
  theme_minimal()

ggplot(df, aes(x = cfswrecent, y = cfswever)) +
  geom_point() +
  theme_minimal()

dev.off()

#' Baseline model:
#' * intercept
#' * age x country random effects (IID)
#' * country random effects (IID)
formula_baseline <- x_eff ~ 1 +
  f(age_idx, model = "iid", group = iso3_idx, control.group = list(model = "iid"),
    constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01)) +
  f(iso3_idx, model = "iid", constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01))

#' Model 1:
#' * space random effects (IID)
formula1 <- update(formula_baseline,
  . ~ . + f(area_idx, model = "iid", constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01))
)

#' Model 2:
#' * space random effects (Besag)
formula2 <- update(formula_baseline,
  . ~ . + f(area_idx, model = "besag", graph = adjM, scale.model = TRUE, constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01))
)

#' Model 3: Model 1, cfswrecent
formula3 <- update(formula1, ". ~ . + cfswrecent")

#' Model 4: Model 2, cfswrecent
formula4 <- update(formula2, ". ~ . + cfswrecent")

#' Model 5: Model 1, cfswever
formula5 <- update(formula1, ". ~ . + cfswever")

#' Model 6: Model 2, cfswever
formula6 <- update(formula2, ". ~ . + cfswever")

formulas <- parse(text = paste0("list(", paste0("formula", 1:6, collapse = ", "), ")")) %>% eval()
models <- paste0("Model ", 1:6) %>% as.list()

#' tryCatch version for safety
try_logistic_model <- function(...) {
  return(tryCatch(logistic_model(...), error = function(e) {
    message("Error!")
    return(NULL)
  }))
}

#' Fit the models
res <- purrr::pmap(
  list(formula = formulas, model_name = models),
  try_logistic_model
)

#' Extract the df and the full fitted models
res_df <- lapply(res, "[[", 1) %>% bind_rows()
res_fit <- lapply(res, "[[", 2)

#' Artefact: Fitted model objects
saveRDS(res_fit, "fsw-logit-sae-fits.rds")

#' Artefact: Smoothed district indicator estimates for logistic regression models
res_df <- res_df %>%
  #' Remove superfluous INLA indicator columns
  select(-area_idx, -age_idx, -sur_idx) %>%
  #' Make it clear which of the estimates are raw and which are from the model (smoothed)
  rename(
    estimate_raw = estimate,
    estimate_smoothed = prob_mean,
    median_smoothed = prob_median,
    ci_lower_smoothed = prob_lower,
    ci_upper_smoothed = prob_upper
  ) %>%
  relocate(model, .before = estimate_smoothed)

write_csv(res_df, "fsw-logit-sae.csv", na = "")

#' Create plotting data
res_plot <- res_df %>%
  filter(!(area_id %in% iso3)) %>%
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
pdf("fsw-logit-sae.pdf", h = 11, w = 8.25)

res_plot %>%
  split(~model) %>%
  lapply(function(x)
    x %>%
      mutate(
        age_group = fct_recode(age_group,
            "15-19" = "Y015_019",
            "20-24" = "Y020_024",
            "25-29" = "Y025_029"
        ),
        source = fct_relevel(source, "raw", "smoothed") %>%
          fct_recode("Survey raw" = "raw", "Smoothed" = "smoothed")
      ) %>%
      ggplot(aes(fill = estimate)) +
      geom_sf(size = 0.1, colour = scales::alpha("grey", 0.25)) +
      scale_fill_viridis_c(option = "C", label = label_percent(), limits = c(0, 0.6)) +
      facet_grid(source ~ age_group) +
      theme_minimal() +
      labs(
        title = paste0(x$model[1])
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
    model = unlist(models),
    .before = dic
  )

pdf("fsw-logit-information-criteria.pdf", h = 3, w = 6.25)

ic_df %>%
  rename("dic_mean" = "dic", "waic_mean" = "waic", "cpo_mean" = "cpo") %>%
  pivot_longer(
    cols = starts_with(c("dic", "waic", "cpo")),
    names_to = "name",
    values_to = "value"
  ) %>%
  separate(name, into = c("metric", "type"), extra = "merge", fill = "left") %>%
  pivot_wider(
    names_from = "type",
    values_from = "value"
  ) %>%
  mutate(
    metric = fct_recode(metric,
      "DIC" = "dic",
      "WAIC" = "waic",
      "CPO" = "cpo"
    ),
    model = fct_recode(model,
      "L1: IID spatial" = "Model 1",
      "L2: Besag spatial" = "Model 2",
      "L3: IID spatial, cfswever" = "Model 3",
      "L4: Besag spatial, cfswever" = "Model 4",
      "L5: IID spatial, cfswrecent" = "Model 5",
      "L6: Besag spatial, cfswrecent" = "Model 6",
    )
  ) %>%
  split(.$metric) %>%
  lapply(function(x)
    x %>%
      mutate(
        min_idx = (mean == min(mean, na.rm = TRUE)),
        max_idx = (mean == max(mean, na.rm = TRUE)),
        best_idx = ifelse(metric %in% c("WAIC", "DIC"), min_idx, max_idx)
      )
  ) %>%
  bind_rows() %>%
  ggplot(aes(x = model, y = mean, col = model, shape = best_idx)) +
    geom_point(size = 3) +
    geom_errorbar(
      aes(ymin = mean - se, ymax = mean + se),
      stat = "identity", position = "dodge", alpha = 0.4, col = "black", width = 0
    ) +
    facet_wrap(~metric, scales = "free") +
    scale_color_manual(values = multi.utils::cbpalette()) +
    scale_shape_manual(values = c(16, 15)) +
    guides(shape = "none") +
    labs(y = "Value", x = "", col = "") +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 9),
      legend.margin = margin(0, 0, 0, 0)
    )

dev.off()

write_csv(ic_df, "fsw-logit-information-criteria.csv", na = "")

#' Which model has the highest CPO?
which.max(ic_df$cpo)

#' Model 4 at the moment!
res_df_best <- filter(res_df, model == paste0("Model ", which.max(ic_df$cpo)))
res_fit_best <- res_fit[[which.max(ic_df$cpo)]]

#' Artefacts: Best fitted model results and object
write_csv(res_df_best, "best-fsw-logit-sae.csv", na = "")
saveRDS(res_fit_best, "best-fsw-logit-sae-fit.rds")

#' Calculate average FSW proportion (temporarily useful for changing the RR for sexnonregplus)
#' Approximately 10%
res_df %>%
  filter(
    model == "Model 6",
    area_level != 1
  ) %>%
  summarise(
    propsexpaid12m = mean(estimate_smoothed, na.rm = TRUE)
  )

#' Plot smoothed versus raw (best model)
pdf("best-smoothed-vs-raw.pdf", h = 5, w = 6.25)

ggplot(res_df_best, aes(x = estimate_raw, y = estimate_smoothed)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(x = "Raw", y = "Smoothed", title = "L6: Besag spatial, csfwever") +
  lims(x = c(0, 1), y = c(0, 1)) +
  facet_wrap(~iso3) +
  theme_minimal()

dev.off()

pdf("best-smoothed-vs-raw-country.pdf", h = 5, w = 6.25)

res_df_best %>%
  group_by(iso3) %>%
  summarise(
    estimate_raw = mean(estimate_raw, na.rm = TRUE),
    estimate_smoothed = mean(estimate_smoothed, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = estimate_raw, y = estimate_smoothed, col = iso3)) +
  geom_point(alpha = 0.5) +
  theme_minimal() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  lims(x = c(0, 0.2), y = c(0, 0.2)) +
  labs(x = "Raw", y = "Smoothed", col = "ISO3", title = "Proportion of `sexnonregplus` in `sexpaid12m`")

dev.off()

#' Plot chloropleth by country (with best model)
pdf("best-fsw-logit-sae.pdf", h = 11, w = 8.25)

res_df_best %>%
  filter(!(area_id %in% iso3)) %>%
  pivot_longer(
    cols = c(starts_with("estimate")),
    names_to = c(".value", "source"),
    names_pattern = "(.*)\\_(.*)"
  ) %>%
  left_join( #' Use this to make it an sf again
    select(areas, area_id),
    by = "area_id"
  ) %>%
  st_as_sf() %>%
  split(~survey_id) %>%
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
      scale_fill_viridis_c(option = "C", label = label_percent(), limits = c(0, 0.6)) +
      facet_grid(source ~ age_group) +
      theme_minimal() +
      labs(
        title = paste0(x$survey_id[1])
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

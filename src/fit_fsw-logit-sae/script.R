#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("fit_fsw-logit-sae")
# setwd("src/fit_fsw-logit-sae")

analysis_level <- multi.utils::analysis_level()
admin1_level <- multi.utils::admin1_level()
iso3 <- names(analysis_level)

areas <- readRDS("depends/areas.rds")
ind <- read_csv("depends/survey_indicators_sexbehav.csv")
pop <- read_csv("depends/interpolated_population.csv")
cfsw_ever <- read_csv("everpaidforsex-bycountry.csv")
cfsw_recent <- read_csv("recentpaidforsex-bycountry.csv")

#' Set ind$estimate > 1 to 1, as well as ind$estimate < 0 to 0
ind$estimate <- constrain_interval(ind$estimate, lower = 0, upper = 1)

#' Use only the surveys which contain a specific paid sex question
available_surveys <- read_csv("depends/available-surveys.csv")

giftsvar_surveys <- available_surveys %>%
  filter(giftsvar == 1) %>%
  pull(survey_id)

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
  theme_minimal()

dev.off()

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
  #' Only the high and very high risk groups (sexnonregplus includes both)
  indicator = c("sexnonreg", "sexpaid12m"),
  #' Three age groups, plus aggregate category
  age_group = c("Y015_019", "Y020_024", "Y025_029", "Y015_024"),
  #' Both the areas in the model and the aggregate country
  bind_rows(areas_model, country) %>%
    st_drop_geometry() %>%
    select(country, iso3, area_id, area_name, area_level,
           area_idx, area_id_aggr, area_sort_order, center_x, center_y)
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
#'  * age (age_idx)
#'  * survey (sur_idx)
#'  * observation (obs_idx)
df <- mutate(df,
    #' Doing this because want Y015_024 to have ID 4 rather than 2 as it would be otherwise
    age_idx = as.integer(factor(age_group, levels = c("Y015_019", "Y020_024", "Y025_029", "Y015_024"))),
    sur_idx = multi.utils::to_int(survey_id),
    obs_idx = multi.utils::to_int(interaction(age_idx, area_idx, sur_idx)),
  )

#' The proportion of (sexnonreg + sexpaid12m) who are sexpaid12m should have:
#' * sample size x_eff from (sexnonreg + sexpaid12m),
#' * and observations x_eff from sexpaid12m
#' This is a very slow way of doing this: perhaps after this project I should learn datatable or try dtplyr
df <- df %>%
  split(.$obs_idx) %>%
  lapply(function(x)
    x %>%
      #' n_eff_kish is sum of x_eff from sexnonreg and sexpaid12m
      mutate(n_eff_kish = sum(x$x_eff)) %>%
      #' We just want to keep the row for sexpaid12m
      filter(indicator == "sexpaid12m") %>%
      #' And rename it to propsexpaid12m
      mutate(indicator = "propsexpaid12m")
  ) %>%
  bind_rows()

pdf("data-check.pdf", h = 5, w = 8)

df %>%
  left_join( #' Use this to make it an sf again
    select(areas, area_id),
    by = "area_id"
  ) %>%
  mutate(est = x_eff / n_eff_kish) %>%
  st_as_sf() %>%
  ggplot(aes(fill = est)) +
  geom_sf(size = 0.1, colour = scales::alpha("grey", 0.25)) +
  theme_minimal()

dev.off()

#' Check for larger observations than sample size
#' This shouldn't trigger in normal cases!
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

#' The rows of df to be included in the model
df_model <- filter(df, age_group != "Y015_024", !(area_id %in% toupper(iso3)))

pdf("covariate-correlation-check.pdf", h = 5, w = 6.25)

ggplot(df_model, aes(x = cfswrecent)) +
  geom_histogram()

ggplot(df_model, aes(x = cfswever)) +
  geom_histogram()

ggplot(df_model, aes(x = cfswrecent, y = cfswever)) +
  geom_point()

dev.off()

#' Model 1: intercept, age random effects (IID)
formula1 <- x_eff ~ 1 +
  f(age_idx, model = "iid", constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01))

#' Model 2: intercept, age random effects (IID), space random effects (IID)
formula2 <- update(formula1,
  . ~ . + f(area_idx, model = "iid", constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01))
)

#' Model 3: intercept, age random effects (IID), space random effects (Besag)
formula3 <- update(formula1,
  . ~ . + f(area_idx, model = "besag", graph = adjM, scale.model = TRUE, constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01))
)

#' Model 4: Model 1, cfswever
formula4 <- update(formula1, ". ~ . + cfswever")

#' Model 5: Model 2, cfswever
formula5 <- update(formula2, ". ~ . + cfswever")

#' Model 6: Model 3, cfswever
formula6 <- update(formula3, ". ~ . + cfswever")

#' Model 7: Model 1, cfswrecent
formula7 <- update(formula1, ". ~ . + cfswrecent")

#' Model 8: Model 2, cfswrecent
formula8 <- update(formula2, ". ~ . + cfswrecent")

#' Model 9: Model 3, cfswrecent
formula9 <- update(formula3, ". ~ . + cfswrecent")

formulas <- parse(text = paste0("list(", paste0("formula", 1:9, collapse = ", "), ")")) %>% eval()
models <- paste0("Model ", 1:9) %>% as.list()

#' Fit the models
res <- purrr::pmap(
  list(formula = formulas, model_name = models),
  logistic_model
)

#' Extract the df and the full fitted models
res_df <- lapply(res, "[[", 1) %>% bind_rows()
res_fit <- lapply(res, "[[", 2)
res_samples <- lapply(res, "[[", 3)

#' Artefact: Samples from all models
saveRDS(res_samples, "fsw-logit-smoothed-district-sexbehav-samples.rds")

#' Artefact: Smoothed district indicator estimates for logistic regression models
res_df <- res_df %>%
  #' Remove superfluous INLA indicator columns
  select(-area_idx, -age_idx, -obs_idx, -sur_idx) %>%
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
  relocate(model, .before = estimate_smoothed)

write_csv(res_df, "fsw-logit-smoothed-district-sexbehav.csv", na = "")

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

pdf("fsw-logit-smoothed-district-sexbehav.pdf", h = 11, w = 8.25)

res_plot %>%
  split(~model) %>%
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

#' Which model has the lowest DIC?
which.min(ic_df$dic)

#' And WAIC?
which.min(ic_df$waic)

#' Both Model 5 at the moment!
write_csv(filter(res_df, model == "Model 5"), "best-fsw-logit-smoothed-district-sexbehav.csv", na = "")

write_csv(ic_df, "information-criteria.csv", na = "")

#' Calculate average FSW proportion (temporarily useful for changing the RR for sexnonregplus)
#' Approximately 10%
res_df %>%
  filter(
    model == "Model 5",
    area_level != 1
  ) %>%
  summarise(
    propsexpaid12m = mean(estimate_smoothed, na.rm = TRUE)
  )

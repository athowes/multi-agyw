#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("fit_fsw-logit-sae")
# setwd("src/fit_fsw-logit-sae")

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
cfsw_ever <- read_csv("everpaidforsex-bycountry.csv")
cfsw_recent <- read_csv("recentpaidforsex-bycountry.csv")

#' Set ind$estimate > 1 to 1, as well as ind$estimate < 0 to 0
ind$estimate <- constrain_interval(ind$estimate, lower = 0, upper = 1)

#' Use only the surveys which contain a specific paid sex question
available_surveys <- read_csv("depends/available-surveys.csv")

giftsvar_surveys <- available_surveys %>%
  filter(giftsvar == 1) %>%
  pull(survey_id)

ind <- ind %>%
  filter(survey_id %in% giftsvar_surveys)

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
  geom_sf(size = 0.1, colour = scales::alpha("grey", 0.25))

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
      select(country = Country, csfwever = EverPaidSex),
    by = "country"
  ) %>%
  left_join(
    cfsw_recent %>%
      select(country = Country, csfwrecent = PaidSexLast12Months),
    by = "country"
  )

#' Add fake survey IDs
df <- mutate(df, survey_id = replace_na(survey_id, "Missing"))

#' Add indicies for:
#'  * age (age_idx)
#'  * survey (sur_idx)
#'  * observation (obs_idx)
df <- df %>%
  mutate(
    #' Doing this because want Y015_024 to have ID 4 rather than 2 as it would be otherwise
    age_idx = as.integer(factor(age_group, levels = c("Y015_019", "Y020_024", "Y025_029", "Y015_024"))),
    sur_idx = to_int(survey_id),
    obs_idx = to_int(interaction(age_idx, area_idx, sur_idx)),
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
  geom_sf(size = 0.1, colour = scales::alpha("grey", 0.25))

dev.off()

#' Check for larger observations than sample size
#' This shouldn't trigger in normal cases!
if(sum(df$n_eff_kish < df$x_eff) > 0) {
  message(
    paste0(
      "There are ", sum(df$n_eff_kish < df$x_eff), " rows when x_eff > n_eff_kish!",
      "\nThis occurs in the following surveys:\n",
      paste(unique(filter(df, n_eff_kish < x_eff)$survey_id), collapse = ", "),
      "\nx_eff and n_eff_kish have been set to zero in these cases."
    )
  )

  df <- df %>%
    mutate(
      x_eff = ifelse(n_eff_kish < x_eff, 0, x_eff),
      n_eff_kish = ifelse(n_eff_kish < x_eff, 0, n_eff_kish)
    )
}

#' x_eff = 0 and n_eff_kish = 0 is causing INLA to crash (could be investigated further)
#' Take the approach of setting both to NA in this case (no information either way)
df <- df %>%
  mutate(
    x_eff = ifelse(n_eff_kish == 0 & x_eff == 0, NA, x_eff),
    n_eff_kish = ifelse(n_eff_kish == 0 & x_eff == 0, NA, n_eff_kish)
  )

#' The rows of df to be included in the model
df_model <- df %>%
  filter(age_group != "Y015_024", !(area_id %in% toupper(iso3)))

#' Model 1: intercept, covariates, age random effects (IID)
formula1 <- x_eff ~ 1 + csfwever + csfwrecent +
  f(age_idx, model = "iid", constr = TRUE, hyper = tau_pc(x = 0.001, u = 2.5, alpha = 0.01))

#' Model 2: intercept, covariates, age random effects (IID), space random effects (IID)
formula2 <- x_eff ~ 1 + csfwever + csfwrecent +
  f(age_idx, model = "iid", constr = TRUE, hyper = tau_pc(x = 0.001, u = 2.5, alpha = 0.01)) +
  f(area_idx, model = "iid", constr = TRUE, hyper = tau_pc(x = 0.001, u = 2.5, alpha = 0.01))

#' Model 3: intercept, covariates, age random effects (IID), space random effects (Besag)
formula3 <- x_eff ~ 1 + csfwever + csfwrecent +
  f(age_idx, model = "iid", constr = TRUE, hyper = tau_pc(x = 0.001, u = 2.5, alpha = 0.01)) +
  f(area_idx, model = "besag", graph = adjM, scale.model = TRUE, constr = TRUE, hyper = tau_pc(x = 0.001, u = 2.5, alpha = 0.01))

formulas <- list(formula1, formula2, formula3)
models <- list("Model 1", "Model 2", "Model 3")

#' Fit the models
res <- purrr::pmap(
  list(formula = formulas, model_name = models),
  logistic_model
)

#' Extract the df and the full fitted models
res_df <- lapply(res, "[[", 1) %>% bind_rows()
res_fit <- lapply(res, "[[", 2)

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
        source = fct_relevel(source, "raw", "smoothed", "aggregate") %>%
          fct_recode("Survey raw" = "raw", "Smoothed" = "smoothed", "Admin 1 aggregate" = "aggr")
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

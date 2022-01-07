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
  #' Only the high and very high risk groups
  indicator = c("sexnonreg", "sexpaid12m"),
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

#' The proportion of sexnonreg who are sexpaid12m should have sample size x_eff from sexnonreg and observations x_eff from sexpaid12m
#' This is a very slow way of doing this: perhaps after this project I should learn datatable or try dtplyr
df <- df %>%
  split(.$obs_idx) %>%
  lapply(function(x)
    x %>%
      mutate(n_eff_kish = filter(x, indicator == "sexnonreg")$x_eff) %>%
      filter(indicator == "sexpaid12m") %>%
      mutate(indicator = "propsexpaid12m")
  ) %>%
  bind_rows()

#' This shouldn't trigger in normal cases!
message(
  paste0(
    "There are: ", sum(df$n_eff_kish < df$x_eff), " rows when x_eff > n_eff_kish! These rows have been removed."
  )
)

df <- df %>%
  filter(n_eff_kish > x_eff)

#' The rows of df to be included in the model
df_model <- df %>%
  filter(age_group != "Y015_024", !(area_id %in% toupper(iso3)))

#' Model 1: intercept, age random effects (IID)
formula1 <- x_eff ~ 1 +
  f(age_idx, model = "iid", constr = TRUE, hyper = tau_pc(x = 0.001, u = 2.5, alpha = 0.01))

#' Model 2: intercept, age random effects (IID), space random effects (IID)
formula2 <- x_eff ~ 1 +
  f(age_idx, model = "iid", constr = TRUE, hyper = tau_pc(x = 0.001, u = 2.5, alpha = 0.01)) +
  f(area_idx, model = "iid", constr = TRUE, hyper = tau_pc(x = 0.001, u = 2.5, alpha = 0.01))

#' Model 3: intercept, age random effects (IID), space random effects (Besag)
formula3 <- x_eff ~ 1 +
  f(age_idx, model = "iid", constr = TRUE, hyper = tau_pc(x = 0.001, u = 2.5, alpha = 0.01)) +
  f(area_idx, model = "besag", graph = adjM, scale.model = TRUE, constr = TRUE, hyper = tau_pc(x = 0.001, u = 2.5, alpha = 0.01))

logistic_model <- function(formula) {
  fit <- inla(
    formula,
    data = df_model,
    family = 'xbinomial',
    Ntrials = n_eff_kish,
    control.family = list(control.link = list(model = "logit")),
    control.predictor = list(compute = TRUE, link = 1),
    control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, config = TRUE),
    inla.mode = "experimental"
  )
}

#' Testing that it works!
temp <- logistic_model(formula1)

#' Approximately 6% of those with non-regular partners are FSW. Seems roughly right.
plogis(temp$summary.fixed$mean)

#' Uncomment and run the two line below to resume development of this script
orderly::orderly_develop_start("fit_multi-sexbehav-sae")
setwd("src/fit_multi-sexbehav-sae")

analysis_level <- c("MWI" = 5)
admin1_level <- c("MWI" = 1)

stopifnot(iso3 %in% names(analysis_level))
stopifnot(iso3 %in% names(admin1_level))

analysis_level <- analysis_level[iso3]
admin1_level <- admin1_level[iso3]

areas <- read_sf(paste0("depends/", tolower(iso3), "_areas.geojson"))
ind <- read_csv(paste0("depends/", tolower(iso3), "_survey_indicators_sexbehav.csv"))

#' Append an indicator for 1 - sex12m
#' TODO: Move this earlier in the pipeline with a mutate call
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

indicators <- c("nosex12m", "sexcohab", "sexnonreg", "sexpaid12m")

df <- crossing(
  indicator = indicators,
  age_group = c("Y015_019", "Y020_024", "Y025_029", "Y015_024"),
  areas_model %>%
    st_drop_geometry() %>%
    select(area_id, area_name, area_idx, area_id_aggr,
           area_sort_order, center_x, center_y)
)

#' Add district observations

#' Verify that there are no estimates > 1
stopifnot(
  filter(ind, estimate > 1) %>%
    nrow() == 0
)

#' Note that df here has 528 rows: 33 areas, 4 categories, 4 age groups
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

to_int <- function(x) as.integer(as.factor(x))

#' Add age, category, age x category interaction, and observation indicators
df <- df %>%
  mutate(cat_idx = to_int(indicator),
         age_idx = to_int(age_group),
         age_cat_idx = interaction(age_idx, cat_idx),
         #' Not sure if this is the best way to do it for obs_idx
         #' Perhaps it can be added earlier in the pipeline
         obs_idx = to_int(interaction(age_idx, area_idx))) %>%
  arrange(obs_idx)

#' Equal to the number of age groups times the number of areas
df$obs_idx %>% max()

#' Model for the aggregated data, using the Poisson trick
tau_prior <- function(x) list(prec = list(initial = log(x), fixed = TRUE))

formula <- x_eff ~ -1 + f(obs_idx, hyper = tau_prior(0.000001)) +
  f(age_cat_idx, model = "iid", constr = TRUE, hyper = tau_prior(0.001))

fit <- inla(formula, data = df, family = 'xPoisson',
            control.predictor = list(link = 1), control.compute = list(config = TRUE))

df$eta <- fit$summary.linear.predictor$mean

#' Compute probabilities
mean <- zoo::rollapply(df$eta, 4, by = 4, function(x) exp(x) / sum(exp(x)), partial = TRUE, align = "left")
df$mean <- as.vector(t(mean))

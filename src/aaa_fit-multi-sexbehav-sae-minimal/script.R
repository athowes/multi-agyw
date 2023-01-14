#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("aaa_fit_multi-sexbehav-sae-minimal", parameters = list(iso3 = "BWA"))
# setwd("src/aaa_fit_multi-sexbehav-sae-minimal")

sf_use_s2(FALSE)

analysis_level <- multi.utils::analysis_level()
admin1_level <- multi.utils::admin1_level()

stopifnot(iso3 %in% names(analysis_level))
stopifnot(iso3 %in% names(admin1_level))

analysis_level <- analysis_level[iso3]
admin1_level <- admin1_level[iso3]

areas <- read_sf(paste0("depends/", tolower(iso3), "_areas.geojson"))
ind <- read_csv(paste0("depends/", tolower(iso3), "_survey_indicators_sexbehav.csv"))

#' If PHIA surveys excluded then filter them out of the raw data
if(!include_phia) {
  ind <- ind %>%
    mutate(type = substr(survey_id, 8, 11)) %>%
    filter(type != "PHIA")
}

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

#' Three or four category version?
if(three_category) {
  indicators <- c("nosex12m", "sexcohab", "sexnonregplus")
} else {
  indicators <- c("nosex12m", "sexcohab", "sexnonreg", "sexpaid12m")

  #' And use only the surveys which contain a specific paid sex question
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
    pull(survey_id) %>%
    unique()

  ind <- ind %>%
    filter(survey_id %in% giftsvar_surveys)
}

#' Create the scaffolding for the estimates
df <- crossing(
  #' In this model we are using three risk categories (rather than four)
  indicator = indicators,
  #' All of the different surveys
  survey_id = unique(ind$survey_id),
  #' Three age groups
  age_group = c("Y015_019", "Y020_024", "Y025_029"),
  #' The areas in the model
  areas_model %>%
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
      facet_grid(indicator ~ age_group) +
      labs(title = paste0(x$survey_id[1])) +
      theme_minimal()
  })

dev.off()

#' Add indicies for:
df <- df %>%
  mutate(
    #' survey
    sur_idx = multi.utils::to_int(survey_id),
    #' age
    age_idx = multi.utils::to_int(age_group),
    #' category
    cat_idx = multi.utils::to_int(indicator),
    #' survey x category
    sur_cat_idx = multi.utils::to_int(interaction(sur_idx, cat_idx)),
    #' age x category
    age_cat_idx = multi.utils::to_int(interaction(age_idx, cat_idx)),
    #' space x category
    area_cat_idx = multi.utils::to_int(interaction(area_idx, cat_idx)),
    #' space x survey
    area_sur_idx = multi.utils::to_int(interaction(area_idx, sur_idx)),
    #' observation
    obs_idx = multi.utils::to_int(interaction(age_idx, area_idx, sur_idx)),
    #' copies
    area_idx_copy = area_idx,
    sur_idx_copy = sur_idx
  ) %>%
  arrange(obs_idx)

#' Specify the models to be fit

#' Baseline model:
#' * category random effects (IID)
#' * age x category random effects (IID)
formula_baseline <- x_eff ~ -1 + f(obs_idx, model = "iid", hyper = multi.utils::tau_fixed(0.000001)) +
  f(cat_idx, model = "iid", constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01)) +
  f(age_idx, model = "iid", group = cat_idx, control.group = list(model = "iid"),
    constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01))

#' Is more than one survey?
include_temporal <- (length(unique(df$survey_id)) > 1)

if(!include_temporal) {

  #' `formula1` below specifies the space x category random effects to have structure matrix given as
  #' the Kronecker product R_{space x category} = I_{space} (x) I_{cat} = I. An alternative is to define
  #' four separate structure matrices. A difference between these approaches is that the former only
  #' involves a single precision parameter whereas the later includes many.

  #' Model 1:
  #' * space x category random effects (IID)
  formula1 <- update(formula_baseline,
    . ~ . + f(area_idx, model = "iid", group = cat_idx, control.group = list(model = "iid"),
              constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01))
  )

  #' Model 2:
  #' * space x category random effects (Besag)
  formula2 <- update(formula_baseline,
    . ~ . + f(area_idx, model = "besag", graph = adjM, scale.model = TRUE, group = cat_idx,
              control.group = list(model = "iid"), constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01))
  )

  formulas <- list(formula1, formula2)
  models <- list("Model 1", "Model 2")
}

if(include_temporal) {

  #' Model 1:
  #' * space x category random effects (IID)
  #' * survey x category random effects (IID)
  formula1 <- update(formula_baseline,
    . ~ . + f(area_idx, model = "iid", group = cat_idx, control.group = list(model = "iid"),
              constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01)) +
            f(sur_idx, model = "iid", group = cat_idx, control.group = list(model = "iid"),
              constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01))
  )

  #' Model 2:
  #' * space x category random effects (Besag)
  #' * survey x category random effects (IID)
  formula2 <- update(formula_baseline,
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

  #' Model 3:
  #' * space x category random effects (IID)
  #' * survey x category random effects (AR1)
  formula3 <- update(formula_baseline,
    . ~ . + f(area_idx, model = "iid", group = cat_idx, control.group = list(model = "iid"),
              constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01)) +
            f(sur_idx, model = "ar1", group = cat_idx, control.group = list(model = "iid"),
              constr = TRUE, hyper = ar1_group_prior)
  )

  #' Model 4:
  #' * space x category random effects (Besag)
  #' * survey x category random effects (AR1)
  formula4 <- update(formula_baseline,
    . ~ . + f(area_idx, model = "besag", graph = adjM, scale.model = TRUE, group = cat_idx,
              control.group = list(model = "iid"), constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01)) +
            f(sur_idx, model = "ar1", group = cat_idx, control.group = list(model = "iid"),
              constr = TRUE, hyper = ar1_group_prior)
  )

  formulas <- list(formula1, formula2, formula3, formula4)
  models <- list("Model 1", "Model 2", "Model 3", "Model 4")
}

#' Should the interaction models be fit too?
if(include_interactions & include_temporal) {

  #' Model 1x:
  #' * Model 1
  #' * space x survey x category random effects (IID x IID)
  formula1x <- update(formula1,
    . ~ . + f(area_sur_idx, model = "iid", group = cat_idx, control.group = list(model = "iid"),
              constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01))
  )

  #' (This approach is defunct: not computationally efficient!)
  #' Create Besag x IID interaction adjacency matrix
  #' Check the resulting matrix with image()
  # interaction_adjM_2x <- multi.utils::repeat_matrix(adjM, n = length(unique(df$sur_idx)))

  #' Model 2x:
  #' * Model 2
  #' * space x survey x category random effects (Besag x IID)
  formula2x <- update(formula2,
    . ~ . + f(area_idx_copy, model = "besag", graph = adjM, scale.model = TRUE, group = sur_idx, replicate = cat_idx,
              control.group = list(model = "iid"), constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01))
  )

  #' Model 3x:
  #' * Model 3
  #' * space x survey x category random effects (IID x AR1)
  formula3x <- update(formula3,
    . ~ . + f(area_idx_copy, model = "iid", group = sur_idx, replicate = cat_idx,
              control.group = list(model = "ar1", hyper = list(rho = list(prior = "pc.cor1", param = c(0, 0.75)))),
              constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01))
  )

  #' Model 4x:
  #' * Model 4
  #' * space x survey x category random effects (Besag x AR1)
  formula4x <- update(formula4,
    . ~ . + f(area_idx_copy, model = "besag", graph = adjM, scale.model = TRUE, group = sur_idx, replicate = cat_idx,
              control.group = list(model = "ar1", hyper = list(rho = list(prior = "pc.cor1", param = c(0, 0.75)))),
              constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01))
  )

  formulas <- list(formula1, formula1x, formula2, formula2x, formula3, formula3x, formula4, formula4x)
  models <- list("Model 1", "Model 1x", "Model 2", "Model 2x", "Model 3", "Model 3x", "Model 4", "Model 4x")
}

#' Fit the models

#' Number of Monte Carlo samples
S <- 1000

#' If low on computational resources
#' Just fit the model which is best performing (in preliminary versions of this work)
if(lightweight) {
  S <- 100

  if(include_temporal) {
    formulas <- list(formula4)
    models <- list("Model 4")
  }
  else {
    formulas <- list(formula2)
    models <- list("Model 2")
  }
}

#' tryCatch version for safety
try_multinomial_model <- function(...) {
  return(tryCatch(multinomial_model(...), error = function(e) {
    message("Error!")
    return(NULL)
  }))
}

res <- purrr::pmap(
  list(formula = formulas, model_name = models, S = S),
  try_multinomial_model
)

#' Extract the df, full fitted models and samples
res_df <- lapply(res, "[[", 1) %>% bind_rows()
res_fit <- lapply(res, "[[", 2)

saveRDS(res_df, "res_df.rds")
saveRDS(res_fit, "res_fit.rds")

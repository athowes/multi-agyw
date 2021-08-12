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
  #' The three age groups plus intended aggregate
  age_group = c("Y015_019", "Y020_024", "Y025_029", "Y015_024"),
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
#'  * space x category (area_cat_idx)
df <- df %>%
  mutate(cat_idx = to_int(indicator),
         #' Doing this because want Y015_024 to have ID 4 rather than 2 as it would be otherwise
         age_idx = as.integer(factor(age_group, levels = c("Y015_019", "Y020_024", "Y025_029", "Y015_024"))),
         age_cat_idx = interaction(age_idx, cat_idx),
         area_cat_idx = interaction(area_idx, cat_idx),
         #' Is the best way to do it for obs_idx? Perhaps can be added earlier in the pipeline
         obs_idx = to_int(interaction(age_idx, area_idx)) %>%
  arrange(obs_idx)

#' Get age-stratified population total sizes from Naomi model outputs
#' This is required for aggregating the estimates e.g. using 15-19 and 20-24 to create 15-24
#' The data has been pre-filtered to only be for the age-groups and sex we are considering
naomi3 <- readRDS("depends/naomi3.rds")

#' Merge this data into df
df <- df %>%
  left_join(
    naomi3 %>%
      filter(indicator_label == "Population",
             #' Using the most recent estimates for now
             calendar_quarter == max(calendar_quarter),
             #' The country and analysis level of interest
             iso3 == iso3,
             area_level == analysis_level) %>%
      #' More sophisticated approach is to use the distribution of population estimate
      #' rather than just a point estimate (the mean) as we do here for now
      rename(population_mean = mean,
             population_lower = lower,
             population_upper = upper,
             age_group = age_group_label) %>%
      select(age_group, area_name, population_mean) %>%
      #' Rename to match df, allowing join
      mutate(
        age_group = fct_recode(age_group, "Y015_019" = "15-19", "Y020_024" = "20-24",
          "Y025_029" = "25-29", "Y015-024" = "15-24")
      ),
    by = c("age_group", "area_name")
  )

#' Data for the model (df) doesn't include the aggregates (since this is using data twice)
#' So we save them off separately
df_agg <- df %>%
  filter(age_group == "Y015_024")

df <- df %>%
  filter(age_group %in% c("Y015_019", "Y020_024", "Y025_029"))

#' Specify the models to be fit

#' Model 1: category random effects (IID), age x category random effects (IID)
formula1 <- x_eff ~ -1 + f(obs_idx, model = "iid", hyper = tau_prior(0.000001)) +
  f(cat_idx, model = "iid", constr = TRUE, hyper = tau_prior(0.001)) +
  f(age_cat_idx, model = "iid", constr = TRUE, hyper = tau_prior(0.001))

#' Kronecker products:
#' If A (m x n) and B (p x q) are matrices then their Kronecker product C (pm x qn) is the block matrix
#'
#' C = [a_11 B ... a_1n B]
#'     [...    ...    ...]
#'     [a_m1 B ... a_mn B]
#'
#' `formula2` below specifies the space x category random effects to have structure matrix given as
#' the Kronecker product R_{space x category} = I_{space} (x) I_{cat} = I. An alternative is to define
#' four separate structure matrices. A difference between these approaches is that the former only
#' involves a single precision parameter whereas the later includes many.

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
  f(area_idx, model = "besag", graph = adjM, group = cat_idx, scale.model = TRUE,
    control.group = list(model = "iid"), constr = TRUE, hyper = tau_prior(0.001))

#' All of the possible models
all_formulas <- parse(text = paste0("list(", paste0("formula", 0:5, collapse = ", "), ")")) %>% eval()
all_models <- list("Model 0", "Model 1: Constant", "Model 2: IID", "Model 3: BYM2", "Model 4: IID (grouped)", "Model 5: BYM2 (grouped)")

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
#' res_df has the 15-24 category too
res_df <- bind_cols(
  res_df,
  lapply(res_fit,
         function(fit) {
           return(data.frame(
             local_dic = fit$dic$local.dic,
             local_waic = fit$waic$local.waic
           ))
         }
  ) %>%
    bind_rows() %>%
    #' Being safe here and explictly adding the NA entires for df_agg
    bind_rows(data.frame(
      local_dic = rep(NA, max_model_id * nrow(df_agg)),
      local_waic = rep(NA, max_model_id * nrow(df_agg))
    ))
)

#' Prepare data for writing to output
res_df <- res_df %>%
  #' Remove superfluous INLA indicator columns
  select(-area_idx, -cat_idx, -age_idx, -obs_idx, -age_cat_idx, -area_cat_idx) %>%
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

#' Cloropleths

pdf("multinomial-smoothed-district-sexbehav.pdf", h = 11, w = 8.5)

lapply(res_plot, function(x)
  x %>%
    mutate(
      age_group = fct_relevel(age_group, "Y015_024") %>%
        fct_recode("15-19" = "Y015_019", "20-24" = "Y020_024", "25-29" = "Y025_029", "15-24" = "Y015_024"),
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

#' Stacked proportion plots

pdf("stacked-proportions.pdf", h = 10, w = 12)

cbpalette <- c("#56B4E9","#009E73", "#E69F00", "#F0E442","#0072B2","#D55E00","#CC79A7", "#999999")

#' In this plot, it'd be great if there was some way to label that 1, 2, 3, ... refer to models,
#' though I think it could reasonably be done in the description for the figure.
#'
#' It might also be nice to include uncertainty, with some options discussed in this thread:
#' https://twitter.com/solomonkurz/status/1372632774285348864
#' However, I do not see that any of the options are good for this plot, and there is already a
#' lot of information being displayed without adding the uncertainty.
res_df %>%
  mutate(
    age_group = fct_relevel(age_group, "Y015_024", after = 3) %>%
      fct_recode(
        "15-19" = "Y015_019",
        "20-24" = "Y020_024",
        "25-29" = "Y025_029",
        "15-24" = "Y015_024"
      ),
    model = fct_recode(model,
      "1" = "Model 1: Constant",
      "2" = "Model 2: IID",
      "3" = "Model 3: BYM2",
      "4" = "Model 4: IID (grouped)",
      "5" = "Model 5: BYM2 (grouped)"
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
  theme_minimal() +
  scale_color_manual(values = cbpalette) +
  labs(title = paste0(res_df$survey_id[1], ": posterior category mean proportions by model")) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.key.width = unit(4, "lines"),
    strip.placement = "outside",
    strip.text.x = element_text(angle = 90, hjust = 0)
  )

dev.off()

#' pdf("bym2-proportions.pdf", h = 11, w = 8.5)
#'
#' if(max_model_id >= 3) {
#'
#' #' Check the mixing parameters in the BYM2 model
#' lapply(1:4, function(i) {
#'   bym2_fit <- res_fit[[3]]
#'   mean <- bym2_fit$summary.hyperpar[i, 1] %>% round(digits = 3)
#'   sd <- bym2_fit$summary.hyperpar[i, 2] %>% round(digits = 3)
#'   bym2_fit$marginals.hyperpar[[i]] %>%
#'     as.data.frame() %>%
#'     ggplot(aes(x = x, y = y)) +
#'     geom_line() +
#'     labs(title = paste0(res_df$survey_id[1], ": posterior of the BYM2 proportion parameter in category ", i),
#'          subtitle = paste0("Mean: ", mean, ", SD: ", sd),
#'          x = "Proportion", y = "p(Proportion)") +
#'     theme_minimal() +
#'     theme(plot.title = element_text(face = "bold"))
#'   })
#'
#' }
#'
#' dev.off()

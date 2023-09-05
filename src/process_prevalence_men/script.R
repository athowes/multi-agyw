#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("process_prevalence_men")
# setwd("src/process_prevalence_men")

priority_iso3 <- multi.utils::priority_iso3()
analysis_level <- multi.utils::analysis_level()

df_3p1 <- read_csv("depends/adjust-best-3p1-multi-sexbehav-sae.csv")
areas <- readRDS("depends/areas.rds")
naomi <- readRDS("depends/naomi.rds")
prev <- read_csv("depends/hiv_indicators_sexbehav.csv")

# for french
naomi$indicator[naomi$indicator=="Nouvelles infections"] <- "New infections"

kps <- readRDS("kplhiv_art.rds")

kp_prev <- bind_rows(kps$MSM$area %>% filter(indicator=="prev" & iso3 %in% priority_iso3) %>%
                       mutate(kp = "MSM"),
                     kps$PWID$area %>% filter(indicator=="prev" & iso3 %in% priority_iso3) %>%
                       mutate(kp = "PWID"))

kp_prev <- kp_prev %>%
  select(-indicator,-lower, - upper) %>%
  mutate(median = log(median / (1-median)),
         area_level = as.numeric(substr(area_id,5,5)))

# no data for cabo delgado - fix temporarily w/ mean for country
kp_prev <- rbind(kp_prev,
                 c("MOZ","MOZ_1_10",mean(kp_prev$median[kp_prev$iso3=="MOZ" & kp_prev$kp=="MSM"]),
                   "MSM",1),
                 c("MOZ","MOZ_1_10",mean(kp_prev$median[kp_prev$iso3=="MOZ" & kp_prev$kp=="PWID"]),
                   "PWID",1))
kp_prev$median <- as.numeric(kp_prev$median)
kp_prev$area_level <- as.numeric(kp_prev$area_level)

kp_prev <- kp_prev %>%
  left_join(
    naomi %>%
      filter(age_group=="15-24") %>%
      pivot_wider(
        values_from = "estimate",
        names_from = "indicator"
      ) %>%
      mutate(logit_gen_prev = log((PLHIV / Population) / (1 - (PLHIV / Population)))) %>%
      select(area_id, logit_gen_prev)
  ) %>%
  select(kp, iso3, area_id, logit_gen_prev, median, area_level)

kp_prev$iso3 <- as.character(kp_prev$iso3)

# fill something in for Haiti since not available in Oli's analysis
kp_prev <- rbind(kp_prev,
                     # data for MSM coming from 2019 KP atlas estimate + aidsinfo men's
                     # prevalence 15-49 2019 (accessed 5 feb 2023)
                     c("MSM","HTI","HTI",log(0.015/(1-0.015)),log(0.045/(1-0.045)),0),
                     # no data for PWID - currently using median PWID prevalence for
                     # areas that are within +/- 0.1 logit prevalence of Haiti
                     c("PWID","HTI","HTI",
                       log(0.045/(1-0.045)),
                       median(kp_prev$median[kp_prev$kp=="PWID" & kp_prev$logit_gen_prev>=(log(0.015/(1-0.015))-0.1) & kp_prev$logit_gen_prev<=(log(0.015/(1-0.015))+0.1)]),0))

kp_prev$logit_gen_prev <- as.numeric(kp_prev$logit_gen_prev)
kp_prev$median <- as.numeric(kp_prev$median)
kp_prev$area_level <- as.numeric(kp_prev$area_level)

### GET THE APPROPRIATE AREA TO MERGE TO KP ESTIMATES

# map our area ids in df_3p1 data frame to their level 1 and 2
areas$temp_id <- areas$area_id
areas$temp_id2 <- areas$area_id
areas$level1 <- ifelse(substr(areas$temp_id,5,5)==1,areas$temp_id,NA)
areas$level2 <- ifelse(substr(areas$temp_id,5,5)==2,areas$temp_id,NA)
while(sum(is.na(areas$temp_id2))!=nrow(areas)) {
  areas$temp_id2 <- factor(areas$temp_id, levels = areas$area_id, labels = areas$parent_area_id)
  areas$temp_id2 <- ifelse(nchar(as.character(areas$temp_id2))==3,NA,as.character(areas$temp_id2))
  areas$temp_id <- ifelse(is.na(areas$temp_id2),areas$temp_id,areas$temp_id2)
  areas$level1 <- ifelse(substr(areas$temp_id,5,5)==1,areas$temp_id,areas$level1)
  areas$level2 <- ifelse(substr(areas$temp_id,5,5)==2,areas$temp_id,areas$level2)
}
areas$level0 <- substr(areas$area_id,1,3)
areas <- areas %>% select(area_id, level0, level1 , level2)

df_3p1 <- df_3p1 %>%
  left_join(areas)

kp_level <- kp_prev %>%
  group_by(iso3) %>%
  summarise(area_level_kp = mean(area_level))

kp_analysis_level <- kp_level$area_level_kp
names(kp_analysis_level) <- kp_level$iso3

df_3p1 <- df_3p1 %>%
  left_join(
    as.data.frame(kp_analysis_level) %>%
      tibble::rownames_to_column("iso3"),
    by = "iso3"
  ) %>%
  mutate(kp_match_area = case_when(kp_analysis_level == 0 ~ level0,
                                   kp_analysis_level == 1 ~ level1,
                                   kp_analysis_level == 2 ~ level2,
                                   TRUE ~ NA_character_))

df_3p1 <- df_3p1 %>%
  select(!(level0:kp_analysis_level))

#

prev_wide <- prev %>%
  filter(
    (nosex12m != 0) & (sexcohab != 0) & (sexnonreg != 0) & (sexpaid12m != 0),
    !age_group %in% c("Y015_024","Y015_049","Y025_049"),
    indicator == "prevalence"
  ) %>%
  mutate(
    behav = case_when(
      nosex12m == 1 ~ "nosex12m", sexcohab == 1 ~ "sexcohab",
      sexnonreg == 1 ~ "sexnonreg", sexpaid12m == 1 ~ "sexpaid12m",
      TRUE ~ "all"
    ), .after = indicator
  ) %>%
  select(indicator, behav, survey_id, area_id, age_group, estimate) %>%
  pivot_wider(
    names_from = "behav",
    values_from = "estimate",
  )

ind <- prev_wide %>%
  mutate(
    #' Calculate the odds
    across(nosex12m:all, ~ .x / (1 - .x), .names = "{.col}_odds"),
    #' Log odds
    across(nosex12m:all, ~ log(.x / (1 - .x)), .names = "{.col}_logodds"),
    #' Prevalence ratios
    across(nosex12m:all, ~ .x / all, .names = "{.col}_pr"),
    #' Odds ratios
    across(nosex12m:all, ~ (.x / (1 - .x)) / all_odds, .names = "{.col}_or")
  ) %>%
  rename_with(.cols = nosex12m:all, ~ paste0(.x, "_prevalence")) %>%
  select(-indicator) %>%
  pivot_longer(
    cols = starts_with(c("nosex12m", "sexcohab", "sexnonreg", "sexpaid12m", "all")),
    names_to = "indicator",
    values_to = "estimate"
  ) %>%
  separate(indicator, into = c("behav", "indicator"))

pdf("prev-data.pdf", h = 7, w = 6.25)

ind %>%
  split(.$indicator) %>%
  lapply(function(x)
  ggplot(x, aes( x = "", y = estimate)) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(aes(col = area_id), alpha = 0.5) +
    facet_grid(age_group ~ behav) +
    labs(title = paste0(x$indicator[1]), x = "") +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      plot.title = element_text(face = "bold"),
      legend.position = "bottom"
    )
  )

while (!is.null(dev.list()))  dev.off()

#' #' Imported from the prevalence ratio tab of Katie's spreadsheet
#' katie_prev_pr <- read_excel(
#'   "Draft outline of size estimation approach v4.xlsx",
#'   sheet = "Prev Ratios",
#'   skip = 1
#' ) %>%
#'   select(-`YWKPs*`)
#'
#' rg <- c("nosex12m", "sexcohab", "sexnonreg", "sexpaid12m")
#' names(katie_prev_pr) <- c("country", "iso3", paste0("prev_", rg), paste0("pr_", rg))
#'
#' katie_pr <- katie_prev_pr %>%
#'   select(iso3, starts_with("pr_")) %>%
#'   filter(iso3 %in% priority_iso3) %>%
#'   pivot_longer(
#'     cols = starts_with("pr_"),
#'     names_to = c("indicator", "behav"),
#'     names_sep = "_",
#'     values_to = "estimate"
#'   ) %>%
#'   rename(area_id = iso3)
#'
#' #' Compare to prevalence ratios from this analysis
#' pdf("katie-comp.pdf", h = 7, w = 6.25)
#'
#' ind %>%
#'   filter(indicator == "pr") %>%
#'   ggplot(aes(x = "", y = estimate)) +
#'     geom_jitter(width = 0.2, alpha = 0.5, aes(col = age_group)) +
#'     geom_point(data = katie_pr, aes(x = "", y = estimate), col = "black", shape = 2) +
#'     facet_grid(area_id ~ behav, scales = "free") +
#'     scale_color_manual(values = multi.utils::cbpalette()) +
#'     labs(x = "") +
#'     theme_minimal()
#'
#' while (!is.null(dev.list()))  dev.off()
#'
#' #' YWKP prevalence ratios
#' katie_ywkp <- read_excel(
#'   "Draft outline of size estimation approach v4.xlsx",
#'   sheet = "HIV YWKPs",
#'   col_names = FALSE,
#'   col_types = c("text", "text", "text", "text",
#'                 "numeric", "numeric", "numeric",
#'                 "numeric", "numeric", "numeric",
#'                 "numeric", "numeric", "numeric",
#'                 "numeric"),
#'   skip = 6
#' ) %>%
#'   filter(!is.na(`...1`)) %>%
#'   select(where(function(x) any(!is.na(x)))) %>%
#'   select(-c(`...13`, `...14`))
#'
#' names(katie_ywkp) <- c(paste0("country", 1:4), "prev_ywkp_under25", "prev_fsw", "prev_under25", "prev")
#'
#' #' Add log-odds (and put prevalences in [0, 1] rather than [0, 100])
#' katie_ywkp <- katie_ywkp %>%
#'   mutate(
#'     across(prev_ywkp_under25:prev, ~ .x / 100, .names = "{.col}"),
#'     across(prev_ywkp_under25:prev, ~ log(.x / (1 - .x)), .names = "{.col}_logodds"),
#'     pr_ywkp_under25 = prev_ywkp_under25 / prev_under25,
#'     pr_fsw = prev_fsw / prev,
#'     lor_fsw = prev_fsw_logodds / prev_logodds
#'   )
#'
#' pdf("ywkp-prev.pdf", h = 5, w = 6.25)
#'
#' # PLOTS FOR ALL FSW
#'
#' katie_ywkp %>%
#'   ggplot(aes(x = prev_logodds, y = prev_fsw_logodds)) +
#'     geom_point() +
#'     geom_smooth(method = "lm") +
#'     geom_smooth(method = "loess", col = "red") +
#'     theme_minimal()
#'
#' katie_ywkp %>%
#'   ggplot(aes(x = prev, y = lor_fsw)) +
#'   geom_point() +
#'   geom_smooth(method = "loess") +
#'   theme_minimal()
#'
#' katie_ywkp %>%
#'   ggplot(aes(x = prev, y = pr_fsw)) +
#'   geom_point() +
#'   geom_smooth(method = "loess") +
#'   theme_minimal()
#'
#' katie_ywkp %>%
#'   ggplot(aes(x = prev, y = prev_fsw)) +
#'   geom_point() +
#'   geom_smooth(method = "loess") +
#'   theme_minimal()
#'
#' ## PLOTS FOR FSW UNDER 25
#'
#' katie_ywkp %>%
#'   ggplot(aes(x = prev_under25_logodds, y = prev_ywkp_under25_logodds)) +
#'   geom_point() +
#'   geom_smooth(method = "lm") +
#'   geom_smooth(method = "loess", col = "red") +
#'   theme_minimal()
#'
#' katie_ywkp %>%
#'   ggplot(aes(x = prev_under25, y = lor_ywkp_under25)) +
#'   geom_point() +
#'   geom_smooth(method = "loess") +
#'   theme_minimal()
#'
#' katie_ywkp %>%
#'   ggplot(aes(x = prev_under25, y = pr_ywkp_under25)) +
#'   geom_point() +
#'   geom_smooth(method = "loess") +
#'   theme_minimal()
#'
#' katie_ywkp %>%
#'   ggplot(aes(x = prev_under25, y = prev_ywkp_under25)) +
#'   geom_point() +
#'   geom_smooth(method = "loess") +
#'   theme_minimal()
#'
#' while (!is.null(dev.list()))  dev.off()
#'
#' ywkp_fit <- lm(prev_fsw_logodds ~ prev_logodds, data = katie_ywkp)
#'
#' data.frame(
#'   prev = seq(0, 0.7, by = 0.001),
#'   pr_ywkp = calculate_ywkp_pr_lor(seq(0, 0.7, by = 0.001))$pr
#' ) %>%
#'   ggplot(aes(x = prev, y = pr_ywkp)) +
#'     geom_line() +
#'     theme_minimal() +
#'     labs(x = "General population prevalence", y = "PR (YWKP)")

#' Calculating the rest of the LOR with logisitic regression
ind_inla <- ind %>%
  mutate(
    nosex12m_id = ifelse(behav == "nosex12m", 1, 0),
    sexcohab_id = ifelse(behav == "sexcohab", 1, 0),
    sexnonreg_id = ifelse(behav == "sexnonreg", 1, 0),
    sexpaid12m_id = ifelse(behav == "sexpaid12m", 1, 0),
    all_id = ifelse(behav == "all", 1, 0)
  ) %>%
  filter(
    indicator == "prevalence",
    !is.na(estimate)
  )

formula_baseline <- estimate ~ -1 + all_id + nosex12m_id + sexcohab_id + sexnonreg_id + sexpaid12m_id

fit_y <- inla(
  formula_baseline,
  control.family = list(link = "logit"),
  control.predictor = list(link = 1, compute = TRUE),
  data = ind_inla %>% filter(age_group %in% c("Y015_019","Y020_024","Y025_029")),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, config = TRUE),
  inla.mode = "experimental"
)

contents <- fit_y$misc$configs$contents
fixed_effects_idx <- contents$start[contents$tag != "Predictor"]
samples <- inla.posterior.sample(n = 1000, fit_y)
fixed_effects <-  lapply(samples, function(x) x$latent[fixed_effects_idx])
fixed_effects <- matrix(unlist(fixed_effects), byrow = T, nrow = length(fixed_effects))
odds_estimate <- colMeans(exp(fixed_effects))
exp(fit_y$summary.fixed$mean) #' This is what you'd get without sampling from the posterior
or_y <- odds_estimate / odds_estimate[1] #' Odds ratio
lor_y <- log(odds_estimate / odds_estimate[1]) #' Log odds ratio
lor_y <- lor_y[-1] #' Don't need leading zero (baseline all)

fit_o <- inla(
  formula_baseline,
  control.family = list(link = "logit"),
  control.predictor = list(link = 1, compute = TRUE),
  data = ind_inla %>% filter(age_group %in% c("Y030_034","Y035_039","Y040_044","Y045_49")),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, config = TRUE),
  inla.mode = "experimental"
)

contents <- fit_o$misc$configs$contents
fixed_effects_idx <- contents$start[contents$tag != "Predictor"]
samples <- inla.posterior.sample(n = 1000, fit_o)
fixed_effects <-  lapply(samples, function(x) x$latent[fixed_effects_idx])
fixed_effects <- matrix(unlist(fixed_effects), byrow = T, nrow = length(fixed_effects))
odds_estimate <- colMeans(exp(fixed_effects))
exp(fit_o$summary.fixed$mean) #' This is what you'd get without sampling from the posterior
or_o <- odds_estimate / odds_estimate[1] #' Odds ratio
lor_o <- log(odds_estimate / odds_estimate[1]) #' Log odds ratio
lor_o <- lor_o[-1] #' Don't need leading zero (baseline all)

#' Naomi estimates of PLHIV and population by district and age band
naomi <- naomi %>%
  pivot_wider(
    names_from = indicator,
    values_from = estimate
  ) %>%
  filter(age_group %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49")) %>%
  mutate(
    age_group = case_when(
      age_group == "15-19" ~ "Y015_019",
      age_group == "20-24" ~ "Y020_024",
      age_group == "25-29" ~ "Y025_029",
      age_group == "30-34" ~ "Y030_034",
      age_group == "35-39" ~ "Y035_039",
      age_group == "40-44" ~ "Y040_044",
      age_group == "45-49" ~ "Y045_049"
    )
  ) %>%
  rename(
    population = Population,
    plhiv = PLHIV,
    infections = `New infections`
  )

#' Modelled estimates of proportion in each risk group
df_3p1 <- df_3p1 %>%
  filter(year == 2018) %>%
  select(area_id, kp_match_area, age_group, indicator, estimate_smoothed) %>%
  pivot_wider(
    names_from = indicator,
    values_from = estimate_smoothed,
    values_fn = mean,
    names_prefix = "prop_"
  )

#' Merge the datasets
df_3p1 <- naomi %>%
  left_join(
    df_3p1,
    by = c("area_id", "age_group")
  ) %>%
  filter(!is.na(prop_nosex12m)) %>%
  # left_join(
  #   select(katie_prev_pr, -starts_with("prev_")),
  #   by = "iso3"
  # ) %>%
  mutate(
    population_nosex12m = population * prop_nosex12m,
    population_sexcohab = population * prop_sexcohab,
    population_sexnonreg = population * prop_sexnonreg,
    population_msm = population * prop_msm,
    population_pwid = population * prop_pwid
  )

#' #' Calculate prevalence and PLHIV using linear disaggregation
#' df_3p1_linear <- df_3p1 %>%
#'   mutate(
#'     prev_nosex12m = plhiv / (population_nosex12m +
#'                              pr_sexcohab * population_sexcohab +
#'                              pr_sexnonreg * population_sexnonreg +
#'                              pr_sexpaid12m * population_sexpaid12m),
#'     prev_sexcohab = pr_sexcohab * prev_nosex12m,
#'     prev_sexnonreg = pr_sexnonreg * prev_nosex12m,
#'     prev_sexpaid12m = pr_sexpaid12m * prev_nosex12m,
#'     plhiv_nosex12m = prev_nosex12m * population_nosex12m,
#'     plhiv_nosex12m = prev_sexcohab * population_sexcohab,
#'     plhiv_nosex12m = prev_sexnonreg * population_sexnonreg,
#'     plhiv_nosex12m = prev_sexpaid12m * population_sexpaid12m
#'   )
#'
#' write_csv(df_3p1_linear, "prev-district-sexbehav-linear.csv")

kp_prev <- kp_prev %>%
  filter(!is.na(area_id)) %>%
  pivot_wider(names_from = kp,
              values_from = c("logit_gen_prev","median")) %>%
  mutate(msm_lor = median_MSM - logit_gen_prev_MSM,
         pwid_lor = median_PWID - logit_gen_prev_PWID) %>%
  select(-c("logit_gen_prev_PWID","logit_gen_prev_MSM","median_PWID","median_MSM","area_level"))

start_time <- Sys.time()

df_3p1_logit <- df_3p1 %>%
  left_join(
    kp_prev,
    by = c("iso3","kp_match_area" = "area_id")
  ) %>%
  select(-kp_match_area) %>%
  pivot_longer(
    cols = starts_with(c("population_", "prop_")),
    names_to = "indicator",
    values_to = "estimate"
  ) %>%
  separate(
    indicator,
    into = c("indicator", "behav")
  ) %>%
  filter(behav %in% c("nosex12m", "sexcohab", "sexnonreg", "msm", "pwid")) %>%
  split(~ area_id + age_group) %>%
  lapply(function(x) {
    if(x$age_group[1] %in% c("Y015_019","Y020_024","Y025_029")) {lor <- lor_y} else {lor <- lor_o}
    population_fine <- filter(x, indicator == "population")$estimate
    plhiv <- x$plhiv[1]
    msm_lor <- x$msm_lor[1]
    pwid_lor <- x$pwid_lor[1]
    lor[4:5] <- c(msm_lor,pwid_lor)
    prev <- logit_scale_prev(lor, population_fine, plhiv)
    y <- filter(x, indicator == "prop") %>%
      mutate(
        indicator = "prev",
        estimate = prev
      )
    bind_rows(x, y)
  }) %>%
  bind_rows()

end_time <- Sys.time()

end_time - start_time

#' Check how the YWKP LORs look
plot(df_3p1_logit$msm_lor)
plot(df_3p1_logit$pwid_lor)

df_3p1_logit <- df_3p1_logit %>%
  unite("indicator", indicator, behav, sep = "_") %>%
  pivot_wider(
    names_from = indicator,
    values_from = estimate
  )

write_csv(df_3p1_logit, "prev-district-sexbehav-logit.csv")

#' Artefact: Cloropleths
# pdf("prev-district-sexbehav-linear.pdf", h = 7, w = 6.25)
#
# df_3p1_linear_plot <- df_3p1_linear %>%
#   select(iso3, area_id, age_group, starts_with("prev_")) %>%
#   pivot_longer(
#     cols = starts_with("prev_"),
#     names_to = "indicator",
#     names_prefix = "prev_",
#     values_to = "prev",
#   ) %>%
#   left_join(
#     select(areas, area_id),
#     by = "area_id"
#   ) %>%
#   st_as_sf()
#
# plotsA <- df_3p1_linear_plot %>%
#   multi.utils::update_naming() %>%
#   split(.$iso3) %>%
#   lapply(function(x)
#     x %>%
#       ggplot(aes(fill = prev)) +
#       geom_sf(size = 0.1, colour = scales::alpha("grey", 0.25)) +
#       coord_sf(lims_method = "geometry_bbox") +
#       scale_fill_viridis_c(option = "C", label = label_percent()) +
#       facet_grid(age_group ~ indicator, labeller = labeller(indicator = label_wrap_gen(20))) +
#       theme_minimal() +
#       labs(
#         title = paste0(x$iso3[1]),
#         fill = "Prevalence"
#       ) +
#       theme(
#         axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         panel.grid = element_blank(),
#         strip.text = element_text(face = "bold"),
#         legend.position = "bottom",
#         legend.key.width = unit(4, "lines")
#       )
#   )
#
# plotsA
#
# while (!is.null(dev.list()))  dev.off()

pdf("prev-district-sexbehav-logit.pdf", h = 7, w = 6.25)

df_3p1_logit_plot <- df_3p1_logit %>%
  select(iso3, area_id, age_group, starts_with("prev_")) %>%
  pivot_longer(
    cols = starts_with("prev_"),
    names_to = "indicator",
    names_prefix = "prev_",
    values_to = "prev",
  ) %>%
  left_join(
    select(areas, area_id),
    by = "area_id"
  ) %>%
  st_as_sf()

plotsB <- df_3p1_logit_plot %>%
  multi.utils::update_naming() %>%
  split(.$iso3) %>%
  lapply(function(x)
    x %>%
      ggplot(aes(fill = prev)) +
      geom_sf(size = 0.1, colour = scales::alpha("grey", 0.25)) +
      coord_sf(lims_method = "geometry_bbox") +
      scale_fill_viridis_c(option = "C", label = label_percent()) +
      facet_grid(age_group ~ indicator, labeller = labeller(indicator = label_wrap_gen(20))) +
      theme_minimal() +
      labs(
        title = paste0(x$iso3[1]),
        fill = "Prevalence"
      ) +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom",
        legend.key.width = unit(4, "lines")
      )
  )

plotsB

while (!is.null(dev.list()))  dev.off()

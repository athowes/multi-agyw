#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("process_prevalence")
# setwd("src/process_prevalence")

priority_iso3 <- multi.utils::priority_iso3()
analysis_level <- multi.utils::analysis_level()

df_3p1 <- read_csv("depends/adjust-best-3p1-multi-sexbehav-sae.csv")
areas <- readRDS("depends/areas.rds")
naomi <- readRDS("depends/naomi.rds")
prev <- read_csv("depends/hiv_indicators_sexbehav.csv")

prev_wide <- prev %>%
  filter(
    (nosex12m != 0) & (sexcohab != 0) & (sexnonreg != 0) & (sexpaid12m != 0),
    age_group != "Y015_024",
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

dev.off()

#' Imported from the prevalence ratio tab of Katie's spreadsheet
katie_prev_pr <- read_excel(
  "Draft outline of size estimation approach v4.xlsx",
  sheet = "Prev Ratios",
  skip = 1
) %>%
  select(-`YWKPs*`)

rg <- c("nosex12m", "sexcohab", "sexnonreg", "sexpaid12m")
names(katie_prev_pr) <- c("country", "iso3", paste0("prev_", rg), paste0("pr_", rg))

katie_pr <- katie_prev_pr %>%
  select(iso3, starts_with("pr_")) %>%
  filter(iso3 %in% priority_iso3) %>%
  pivot_longer(
    cols = starts_with("pr_"),
    names_to = c("indicator", "behav"),
    names_sep = "_",
    values_to = "estimate"
  ) %>%
  rename(area_id = iso3)

#' Compare to prevalence ratios from this analysis
pdf("katie-comp.pdf", h = 7, w = 6.25)

ind %>%
  filter(indicator == "pr") %>%
  ggplot(aes(x = "", y = estimate)) +
    geom_jitter(width = 0.2, alpha = 0.5, aes(col = age_group)) +
    geom_point(data = katie_pr, aes(x = "", y = estimate), col = "black", shape = 2) +
    facet_grid(area_id ~ behav, scales = "free") +
    scale_color_manual(values = multi.utils::cbpalette()) +
    labs(x = "") +
    theme_minimal()

dev.off()

#' YWKP prevalence ratios
katie_ywkp <- read_excel(
  "Draft outline of size estimation approach v4.xlsx",
  sheet = "HIV YWKPs",
  col_names = FALSE,
  col_types = c("text", "text", "text", "text",
                "numeric", "numeric", "numeric",
                "numeric", "numeric", "numeric",
                "numeric", "numeric", "numeric",
                "numeric"),
  skip = 6
) %>%
  filter(!is.na(`...1`)) %>%
  select(where(function(x) any(!is.na(x)))) %>%
  select(-c(`...13`, `...14`))

names(katie_ywkp) <- c(paste0("country", 1:4), "prev_ywkp_under25", "prev_ywkp", "prev_under25", "prev")

#' Add log-odds (and put prevalences in [0, 1] rather than [0, 100])
katie_ywkp <- katie_ywkp %>%
  mutate(
    across(prev_ywkp_under25:prev, ~ .x / 100, .names = "{.col}"),
    across(prev_ywkp_under25:prev, ~ log(.x / (1 - .x)), .names = "{.col}_logodds"),
    pr_ywkp_under25 = prev_ywkp_under25 / prev_under25,
    pr_ywkp = prev_ywkp / prev,
    lor_ywkp_under25 = prev_ywkp_under25_logodds / prev_under25_logodds,
    lor_ywkp = prev_ywkp_logodds / prev_logodds
  )

pdf("ywkp-prev.pdf", h = 5, w = 6.25)

katie_ywkp %>%
  ggplot(aes(x = prev_logodds, y = prev_ywkp_logodds)) +
    geom_point() +
    geom_smooth(method = "lm") +
    geom_smooth(method = "loess", col = "red") +
    theme_minimal()

katie_ywkp %>%
  ggplot(aes(x = prev, y = lor_ywkp)) +
  geom_point() +
  geom_smooth(method = "loess") +
  theme_minimal()

katie_ywkp %>%
  ggplot(aes(x = prev, y = pr_ywkp)) +
  geom_point() +
  geom_smooth(method = "loess") +
  theme_minimal()

katie_ywkp %>%
  ggplot(aes(x = prev, y = prev_ywkp)) +
  geom_point() +
  geom_smooth(method = "loess") +
  theme_minimal()

dev.off()

ywkp_fit <- lm(prev_ywkp_logodds ~ prev_logodds, data = katie_ywkp)

data.frame(
  prev = seq(0, 0.3, by = 0.001),
  pr_ywkp = calculate_ywkp_pr_lor(seq(0, 0.3, by = 0.001))$pr
) %>%
  ggplot(aes(x = prev, y = pr_ywkp)) +
    geom_line() +
    theme_minimal() +
    labs(x = "General population prevalence", y = "PR (YWKP)")

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

fit <- inla(
  formula_baseline,
  control.family = list(link = "logit"),
  control.predictor = list(link = 1, compute = TRUE),
  data = ind_inla,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, config = TRUE),
  inla.mode = "experimental"
)

contents <- fit$misc$configs$contents
fixed_effects_idx <- contents$start[contents$tag != "Predictor"]
samples <- inla.posterior.sample(n = 1000, fit)
fixed_effects <-  lapply(samples, function(x) x$latent[fixed_effects_idx])
fixed_effects <- matrix(unlist(fixed_effects), byrow = T, nrow = length(fixed_effects))
odds_estimate <- colMeans(exp(fixed_effects))
exp(fit$summary.fixed$mean) #' This is what you'd get without sampling from the posterior
or <- odds_estimate / odds_estimate[1] #' Odds ratio
lor <- log(odds_estimate / odds_estimate[1]) #' Log odds ratio
lor <- lor[-1] #' Don't need leading zero (baseline all)

#' Naomi estimates of PLHIV and population by district and age band
naomi <- naomi %>%
  pivot_wider(
    names_from = indicator,
    values_from = estimate
  ) %>%
  filter(age_group %in% c("15-19", "20-24", "25-29")) %>%
  mutate(
    age_group = case_when(
      age_group == "15-19" ~ "Y015_019",
      age_group == "20-24" ~ "Y020_024",
      age_group == "25-29" ~ "Y025_029"
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
  select(area_id, age_group, indicator, estimate_smoothed) %>%
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
  left_join(
    select(katie_prev_pr, -starts_with("prev_")),
    by = "iso3"
  ) %>%
  mutate(
    population_nosex12m = population * prop_nosex12m,
    population_sexcohab = population * prop_sexcohab,
    population_sexnonreg = population * prop_sexnonreg,
    population_sexpaid12m = population * prop_sexpaid12m
  )

#' Calculate prevalence and PLHIV using linear disaggregation
df_3p1_linear <- df_3p1 %>%
  mutate(
    prev_nosex12m = plhiv / (population_nosex12m +
                             pr_sexcohab * population_sexcohab +
                             pr_sexnonreg * population_sexnonreg +
                             pr_sexpaid12m * population_sexpaid12m),
    prev_sexcohab = pr_sexcohab * prev_nosex12m,
    prev_sexnonreg = pr_sexnonreg * prev_nosex12m,
    prev_sexpaid12m = pr_sexpaid12m * prev_nosex12m,
    plhiv_nosex12m = prev_nosex12m * population_nosex12m,
    plhiv_nosex12m = prev_sexcohab * population_sexcohab,
    plhiv_nosex12m = prev_sexnonreg * population_sexnonreg,
    plhiv_nosex12m = prev_sexpaid12m * population_sexpaid12m
  )

write_csv(df_3p1_linear, "prev-district-sexbehav-linear.csv")

start_time <- Sys.time()

df_3p1_logit <- df_3p1 %>%
  mutate(
    gen_prev = plhiv / population,
    ywkp_lor = calculate_ywkp_pr_lor(gen_prev, fit = ywkp_fit)$lor
  ) %>%
  select(-starts_with("pr_"), -gen_prev) %>%
  pivot_longer(
    cols = starts_with(c("population_", "prop_")),
    names_to = "indicator",
    values_to = "estimate"
  ) %>%
  separate(
    indicator,
    into = c("indicator", "behav")
  ) %>%
  filter(behav %in% c("nosex12m", "sexcohab", "sexnonreg", "sexpaid12m")) %>%
  split(~ area_id + age_group) %>%
  lapply(function(x) {
    population_fine <- filter(x, indicator == "population")$estimate
    plhiv <- x$plhiv[1]
    ywkp_lor <- x$ywkp_lor[1]
    lor[4] <- ywkp_lor
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
plot(df_3p1_logit$ywkp_lor)

df_3p1_logit <- df_3p1_logit %>%
  unite("indicator", indicator, behav, sep = "_") %>%
  pivot_wider(
    names_from = indicator,
    values_from = estimate
  )

write_csv(df_3p1_logit, "prev-district-sexbehav-logit.csv")

#' Artefact: Cloropleths
pdf("prev-district-sexbehav-linear.pdf", h = 7, w = 6.25)

df_3p1_linear_plot <- df_3p1_linear %>%
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

plotsA <- df_3p1_linear_plot %>%
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

plotsA

dev.off()

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

dev.off()

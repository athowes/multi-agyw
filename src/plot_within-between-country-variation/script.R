#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("plot_within-between-country-variation")
# setwd("src/plot_within-between-country-variation")

df_adjusted <- read_csv("depends/adjust-best-3p1-multi-sexbehav-sae.csv") %>%
  update_naming()

adjusted <- prepare_estimates(df_adjusted)
df_subnational <- adjusted$df_subnational
df_national <- adjusted$df_national

pdf("within-between-country-variation.pdf", h = 7, w = 8)

df_subnational %>%
  mutate(iso3 = reorder(iso3, iso3_sort_order)) %>%
  ggplot(aes(x = fct_rev(iso3), y = estimate_smoothed, col = region)) +
    geom_jitter(width = 0.1, alpha = 0.6, shape = 20) +
    geom_point(data = df_national, aes(x = fct_rev(iso3), y = estimate_smoothed),
               shape = 21, size = 2, fill = "white", col = "black", alpha = 0.9) +
    facet_grid(age_group ~  indicator) +
    scale_color_manual(values = multi.utils::cbpalette()) +
    scale_y_continuous(labels = function(x) paste0(100 * x, "%")) +
    coord_flip() +
    labs(x = "", y = "Proportion", col = "Regions of sub-Saharan Africa") +
    guides(colour = guide_legend(override.aes = list(alpha = 0.9, size = 5))) +
    theme_minimal() +
    theme(
      panel.spacing = unit(1.5, "lines"),
      legend.position = "bottom",
      strip.text = element_text(face = "bold")
    )

dev.off()

#' Have a closer look at the FSW estimates and check that the Johnston adjustment has worked
df_unadjusted <- read_csv("depends/best-3p1-multi-sexbehav-sae.csv") %>%
  update_naming()
unadjusted <- prepare_estimates(df_unadjusted)

df_subnational <- bind_rows(
  mutate(df_subnational, adjustment = "Johnston"),
  mutate(unadjusted$df_subnational, adjustment = "None")
)

df_national <- bind_rows(
  mutate(df_national, adjustment = "Johnston"),
  mutate(unadjusted$df_national, adjustment = "None")
)

pdf("within-between-country-variation-fsw.pdf", h = 6, w = 6.25)

df_subnational %>%
  filter(indicator == "YWKP") %>%
  mutate(iso3 = reorder(iso3, iso3_sort_order)) %>%
  ggplot(aes(x = fct_rev(iso3), y = estimate_smoothed, col = region)) +
  geom_jitter(width = 0.1, alpha = 0.6, shape = 20) +
  geom_point(data = df_national, aes(x = fct_rev(iso3), y = estimate_smoothed),
             shape = 21, size = 2, fill = "white", col = "black", alpha = 0.9) +
  facet_grid(adjustment ~ age_group) +
  scale_color_manual(values = multi.utils::cbpalette()) +
  scale_y_continuous(labels = function(x) paste0(100 * x, "%")) +
  coord_flip() +
  labs(x = "", y = "Proportion", col = "Regions of sub-Saharan Africa") +
  guides(colour = guide_legend(override.aes = list(alpha = 0.9, size = 5))) +
  theme_minimal() +
  theme(
    panel.spacing = unit(1.5, "lines"),
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )

dev.off()

#' Quantification of points discussed
#' District level quantile information (age disaggregated)
df_subnational %>%
  select(iso3, indicator, age_group, estimate_smoothed) %>%
  group_by(indicator, age_group) %>%
  summarise(
    lower = 100 * quantile(estimate_smoothed, probs = 0.025, na.rm = TRUE),
    median = 100 * median(estimate_smoothed, na.rm = TRUE),
    upper = 100 * quantile(estimate_smoothed, probs = 0.975, na.rm = TRUE)
  )

#' District level quantile information aggregated by age (population weighted)
df_subnational %>%
  select(iso3, area_id, indicator, age_group, estimate_smoothed, population_mean) %>%
  group_by(area_id, indicator) %>%
  summarise(
    estimate_smoothed = sum(estimate_smoothed * population_mean) / sum(population_mean)
  ) %>%
  ungroup() %>%
  group_by(indicator) %>%
  summarise(
    lower = 100 * quantile(estimate_smoothed, probs = 0.025, na.rm = TRUE),
    median = 100 * median(estimate_smoothed, na.rm = TRUE),
    upper = 100 * quantile(estimate_smoothed, probs = 0.975, na.rm = TRUE)
  )

#' What proportion of 15-19 year-olds are cohabiting in MOZ?
df_national %>%
  filter(
    indicator == "Cohabiting partner",
    age_group == "15-19",
    iso3 == "Mozambique"
  ) %>%
  mutate(estimate_smoothed = 100 * estimate_smoothed) %>%
  pull(estimate_smoothed) %>%
  signif(digits = 3)

#' District level quantile information aggregated by age (population weighted)
#' What proportion of 20-29 year-olds are cohabiting versus with nonregular partner(s),
#' above and below the south / east dividing border (UN geoscheme)?
df_subnational %>%
  mutate(
    border = ifelse(region %in% c("East", "Middle"), "Above", "Below")
  ) %>%
  select(iso3, area_id, indicator, age_group, border, estimate_smoothed, population_mean) %>%
  filter(
    indicator %in% c("Cohabiting partner", "Nonregular partner(s)"),
    age_group %in% c("20-24", "25-29")
  ) %>%
  group_by(area_id, indicator) %>%
  summarise(
    estimate_smoothed = sum(estimate_smoothed * population_mean) / sum(population_mean),
    border = max(border) #' Should just be a way to keep border column
  ) %>%
  ungroup() %>%
  group_by(border, indicator) %>%
  summarise(
    lower = 100 * quantile(estimate_smoothed, probs = 0.025, na.rm = TRUE),
    median = 100 * median(estimate_smoothed, na.rm = TRUE),
    upper = 100 * quantile(estimate_smoothed, probs = 0.975, na.rm = TRUE)
  ) %>%
  mutate(across(lower:upper, ~signif(.x, digits = 3)))

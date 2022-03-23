#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("plot_within-between-country-variation")
# setwd("src/plot_within-between-country-variation")

df_3p1 <- read_csv("depends/adjust-best-3p1-multi-sexbehav-sae.csv")
pop <- read_csv("depends/interpolated_population.csv")

#' Add population and updating naming
df_3p1 <- df_3p1 %>%
  left_join(
    filter(pop, sex == "female"),
    by = c("area_id", "year", "age_group")
  ) %>%
  rename(population_mean = population) %>%
  multi.utils::update_naming()

temp_3p1 <- prepare_estimates(df_3p1)
df_3p1_subnational <- temp_3p1$df_subnational
df_3p1_national <- temp_3p1$df_national

pdf("3p1-within-between-country-variation.pdf", h = 7, w = 6.25)

plotA <- df_3p1_subnational %>%
  mutate(iso3 = reorder(iso3, iso3_sort_order)) %>%
  ggplot(aes(x = fct_rev(iso3), y = estimate_smoothed, col = region)) +
    geom_jitter(width = 0.1, alpha = 0.6, shape = 20) +
    geom_point(data = df_3p1_national, aes(x = fct_rev(iso3), y = estimate_smoothed),
               shape = 21, size = 2, fill = "white", col = "black", alpha = 0.9) +
    facet_grid(age_group ~  indicator, labeller = labeller(indicator = label_wrap_gen(10))) +
    scale_color_manual(values = multi.utils::cbpalette()) +
    scale_y_continuous(labels = function(x) paste0(100 * x, "%")) +
    coord_flip() +
    labs(x = "", y = "Proportion", col = "Regions of sub-Saharan Africa") +
    guides(colour = guide_legend(override.aes = list(alpha = 0.9, size = 5))) +
    theme_minimal() +
    theme(
      panel.spacing = unit(0.5, "lines"),
      legend.position = "bottom",
      strip.text = element_text(face = "bold"),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 9),
    )

plotA

dev.off()

#' Save version for poster
ggsave(
  "3p1-within-between-country-variation.png",
  plotA,
  width = 6.25, height = 7, units = "in", dpi = 300
)

#' Have a closer look at the FSW estimates and check that the Johnston adjustment has worked
df_3p1_unadjusted <- read_csv("depends/best-3p1-multi-sexbehav-sae.csv")

#' Add population and updating naming
df_3p1_unadjusted <- df_3p1_unadjusted %>%
  left_join(
    filter(pop, sex == "female"),
    by = c("area_id", "year", "age_group")
  ) %>%
  rename(population_mean = population) %>%
  multi.utils::update_naming()

temp_3p1_unadjusted <- prepare_estimates(df_3p1_unadjusted)

df_subnational_fsw <- bind_rows(
  mutate(df_3p1_subnational, adjustment = "Johnston"),
  mutate(temp_3p1_unadjusted$df_subnational, adjustment = "None")
)

df_national_fsw <- bind_rows(
  mutate(df_3p1_national, adjustment = "Johnston"),
  mutate(temp_3p1_unadjusted$df_national, adjustment = "None")
)

pdf("fsw-within-between-country-variation.pdf", h = 6, w = 6.25)

df_subnational_fsw %>%
  filter(indicator == "FSW") %>%
  mutate(iso3 = reorder(iso3, iso3_sort_order)) %>%
  ggplot(aes(x = fct_rev(iso3), y = estimate_smoothed, col = region)) +
  geom_jitter(width = 0.1, alpha = 0.6, shape = 20) +
  geom_point(data = filter(df_national_fsw, indicator == "FSW"), aes(x = fct_rev(iso3), y = estimate_smoothed),
             shape = 21, size = 2, fill = "white", col = "black", alpha = 0.9) +
  facet_grid(adjustment ~ age_group) +
  scale_color_manual(values = multi.utils::cbpalette()) +
  scale_y_continuous(labels = function(x) paste0(100 * x, "%")) +
  coord_flip() +
  labs(x = "", y = "Proportion", col = "Regions of sub-Saharan Africa") +
  guides(colour = guide_legend(override.aes = list(alpha = 0.9, size = 5))) +
  theme_minimal() +
  theme(
    panel.spacing = unit(1, "lines"),
    legend.position = "bottom",
    strip.text = element_text(face = "bold"),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 9),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

dev.off()

#' Quantification of points discussed
#' District level quantile information (age disaggregated)
df_3p1_subnational %>%
  select(iso3, indicator, age_group, estimate_smoothed) %>%
  group_by(indicator, age_group) %>%
  summarise(
    lower = 100 * quantile(estimate_smoothed, probs = 0.025, na.rm = TRUE),
    median = 100 * median(estimate_smoothed, na.rm = TRUE),
    upper = 100 * quantile(estimate_smoothed, probs = 0.975, na.rm = TRUE)
  )

#' District level quantile information aggregated by age (population weighted)
df_3p1_subnational %>%
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
df_3p1_national %>%
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
df_3p1_subnational %>%
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

#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("plot_infections-averted")
# setwd("src/plot_infections-averted")

df <- read_csv("depends/incidence-district-sexbehav.csv")

#' Assume some value for how much a hypothetical intervention reduces incidence by (multiplicatively)
p <- 1

df_area_age_behav <- df %>%
  select(iso3, area_id, age_group, starts_with("population_sex"), starts_with("incidence_sex")) %>%
  pivot_longer(
    cols = starts_with("population") | starts_with("incidence"),
    names_to = c("indicator", "category"),
    names_sep = "_"
  ) %>%
  pivot_wider(
    id_cols = c("iso3", "area_id", "age_group", "indicator", "category"),
    names_from = "indicator",
    values_from = "value"
  )

df_area_age <- df_area_age_behav %>%
  group_by(area_id, age_group) %>%
  summarise(
    incidence = sum(incidence * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
    population = sum(population, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(iso3 = substr(area_id, 1, 3))

df_area_behav <- df_area_age_behav %>%
  group_by(area_id, category) %>%
  summarise(
    incidence = sum(incidence * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
    population = sum(population, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(iso3 = substr(area_id, 1, 3))

df_age_behav <- df_area_age_behav %>%
  group_by(age_group, category) %>%
  summarise(
    incidence = sum(incidence * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
    population = sum(population, na.rm = TRUE)
  ) %>%
  ungroup()

df_none <- df_area_age_behav %>%
  summarise(
    incidence = sum(incidence * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
    population = sum(population, na.rm = TRUE)
  ) %>%
  ungroup()

inc_area_age_behav <- compute_infections_averted(df_area_age_behav, stratification_name = "Area, age, behaviour")
inc_area_age <- compute_infections_averted(df_area_age, stratification_name = "Area, age")
inc_area_behav <- compute_infections_averted(df_area_behav, stratification_name = "Area, behaviour")
inc_age_behav <- compute_infections_averted(df_age_behav, stratification_name = "Age, behaviour")
inc_none <- compute_infections_averted(df_none, stratification_name = "None")

inc <- bind_rows(
  inc_area_age_behav,
  inc_area_age,
  inc_area_behav,
  inc_age_behav,
  inc_none
)

#' Add "improvement over no targetting" column
infections_averted_per_population_baseline <- inc %>%
  filter(stratification == "None") %>%
  #' Just the end of the line
  filter(population_cumulative == max(population_cumulative)) %>%
  mutate(infections_averted_per_population_baseline = infections_averted_cumulative / population_cumulative) %>%
  pull(infections_averted_per_population_baseline)

inc <- inc %>%
  mutate(
    infections_averted_cumulative_baseline = population_cumulative * infections_averted_per_population_baseline,
    infections_averted_cumulative_improvement = infections_averted_cumulative - infections_averted_cumulative_baseline
  )

pdf("infections-averted.pdf", h = 4, w = 6.25)

ggplot(inc, aes(x = population_cumulative, y = infections_averted_cumulative_improvement, col = stratification)) +
  geom_line(alpha = 0.8, size = 0.7) +
  theme_minimal() +
  scale_color_manual(values = multi.utils::cbpalette()[-c(4, 5)]) +
  scale_y_continuous(labels = label_number(scale = 1e-3)) +
  scale_x_continuous(labels = label_number(scale = 1e-6)) +
  labs(
    x = "Total population reached (10E6)",
    y = "Additional infections reached (10E3)",
    title = "Population stratification targetting comparison",
    col = "Stratification"
  )

dev.off()

#' Separate analysis with each country separate

inc_country_area_age_behav <- compute_infections_averted(df_area_age_behav, stratification_name = "Area, age, behaviour", by_country = TRUE)
inc_country_area_age <- compute_infections_averted(df_area_age, stratification_name = "Area, age", by_country = TRUE)
inc_country_area_behav <- compute_infections_averted(df_area_behav, stratification_name = "Area, behaviour", by_country = TRUE)

df_age_behav <- df_area_age_behav %>%
  group_by(iso3, age_group, category) %>%
  summarise(
    incidence = sum(incidence * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
    population = sum(population, na.rm = TRUE)
  ) %>%
  ungroup()

inc_country_age_behav <- compute_infections_averted(df_age_behav, stratification_name = "Age, behaviour", by_country = TRUE)

df_none <- df_area_age_behav %>%
  group_by(iso3) %>%
  summarise(
    incidence = sum(incidence * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
    population = sum(population, na.rm = TRUE)
  ) %>%
  ungroup()

inc_country_none <- compute_infections_averted(df_none, stratification_name = "None", by_country = TRUE)

inc_country <- bind_rows(
  inc_country_area_age_behav,
  inc_country_area_age,
  inc_country_area_behav,
  inc_country_age_behav,
  inc_country_none
)

#' Add "improvement over no targetting" column
infections_averted_per_population_baseline_country <- inc_country %>%
  filter(stratification == "None") %>%
  group_by(iso3) %>%
  #' Just the end of the line
  filter(population_cumulative == max(population_cumulative)) %>%
  mutate(infections_averted_per_population_baseline = infections_averted_cumulative / population_cumulative) %>%
  select(iso3, infections_averted_per_population_baseline)

inc_country <- inc_country %>%
  left_join(
    infections_averted_per_population_baseline_country,
    by = "iso3"
  ) %>%
  mutate(
    infections_averted_cumulative_baseline = population_cumulative * infections_averted_per_population_baseline,
    infections_averted_cumulative_improvement = infections_averted_cumulative - infections_averted_cumulative_baseline
  )

pdf("infections-averted-country.pdf", h = 7, w = 9)

ggplot(inc_country, aes(x = population_cumulative, y = infections_averted_cumulative_improvement, col = stratification)) +
  geom_line(alpha = 0.8, size = 0.7) +
  facet_wrap(~iso3, scales = "free") +
  theme_minimal() +
  scale_color_manual(values = cbpalette[-c(4, 5)]) +
  scale_y_continuous(labels = label_number(scale = 1e-3)) +
  scale_x_continuous(labels = label_number(scale = 1e-6)) +
  labs(
    x = "Total population reached (10E6)",
    y = "Additional infections reached (10E3)",
    title = "Population stratification targetting comparison",
    col = "Stratification"
  ) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 10)
  )

dev.off()

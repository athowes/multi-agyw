#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("analyze_infections-averted")
# setwd("src/analyze_infections-averted")

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

pdf("infections-averted.pdf", h = 4, w = 6.25)

cbpalette <- c("#56B4E9","#009E73", "#E69F00", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")

ggplot(inc, aes(x = population_cumulative, y = infections_averted_cumulative, col = stratification)) +
  geom_line(linetype = 2) +
  theme_minimal() +
  scale_color_manual(values = cbpalette[-c(4, 5)]) +
  scale_y_continuous(labels = unit_format(unit = "Th", scale = 1e-3)) +
  scale_x_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  labs(
    x = "Total population reached",
    y = "Total infections reached",
    title = "Population stratification targetting comparison",
    col = "Stratification"
  )

dev.off()

#' Separate analysis with each country separate

inc_country_area_age_behav <- compute_infections_averted_by_country(df_area_age_behav, stratification_name = "Area, age, behaviour")
inc_country_area_age <- compute_infections_averted_by_country(df_area_age, stratification_name = "Area, age")
inc_country_area_behav <- compute_infections_averted_by_country(df_area_behav, stratification_name = "Area, behaviour")

df_age_behav <- df_area_age_behav %>%
  group_by(iso3, age_group, category) %>%
  summarise(
    incidence = sum(incidence * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
    population = sum(population, na.rm = TRUE)
  ) %>%
  ungroup()

inc_country_age_behav <- compute_infections_averted_by_country(df_age_behav, stratification_name = "Age, behaviour")

df_none <- df_area_age_behav %>%
  group_by(iso3) %>%
  summarise(
    incidence = sum(incidence * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
    population = sum(population, na.rm = TRUE)
  ) %>%
  ungroup()

inc_country_none <- compute_infections_averted_by_country(df_none, stratification_name = "None")

inc_country <- bind_rows(
  inc_country_area_age_behav,
  inc_country_area_age,
  inc_country_area_behav,
  inc_country_age_behav,
  inc_country_none
)

pdf("infections-averted-country.pdf", h = 7, w = 9)

ggplot(inc_country, aes(x = population_cumulative, y = infections_averted_cumulative, col = stratification)) +
  geom_line(alpha = 0.5) +
  facet_wrap(~iso3, scales = "free") +
  theme_minimal() +
  scale_color_manual(values = cbpalette[-c(4, 5)]) +
  scale_y_continuous(labels = unit_format(unit = "Th", scale = 1e-3)) +
  scale_x_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  labs(
    x = "Total population reached",
    y = "Total infections reached",
    title = "Population stratification targetting comparison",
    col = "Stratification"
  ) +
  theme(
    legend.position = "bottom"
  )

dev.off()

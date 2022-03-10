#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("plot_infections-averted")
# setwd("src/plot_infections-averted")

df <- read_csv("depends/incidence-district-sexbehav.csv")

#' Assume some value for how much a hypothetical intervention reduces incidence by (multiplicatively)
#' A value of one corresponds to "finding" an infection (or infections averted if the intervention is 100% effective)
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

#' https://tidyeval.tidyverse.org/dplyr.html
aggregate_inc_pop <- function(df, ...) {
  df <- df %>%
    group_by(...) %>%
    summarise(
      incidence = sum(incidence * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
      population = sum(population, na.rm = TRUE)
    ) %>%
    ungroup()

  if("area_id" %in% names(df)) {
    df <- mutate(df, iso3 = substr(area_id, 1, 3))
  }

  df
}

df_area_age <- aggregate_inc_pop(df_area_age_behav, area_id, age_group)
df_area_behav <- aggregate_inc_pop(df_area_age_behav, area_id, category)
df_age_behav <- aggregate_inc_pop(df_area_age_behav, age_group, category)
df_area <- aggregate_inc_pop(df_area_age_behav, area_id)
df_age <- aggregate_inc_pop(df_area_age_behav, age_group)
df_behav <-aggregate_inc_pop(df_area_age_behav, category)
df_none <- aggregate_inc_pop(df_area_age_behav)

inc_area_age_behav <- compute_infections_averted(df_area_age_behav, stratification_name = "Area, age, behaviour")
inc_area_age <- compute_infections_averted(df_area_age, stratification_name = "Area, age")
inc_area_behav <- compute_infections_averted(df_area_behav, stratification_name = "Area, behaviour")
inc_age_behav <- compute_infections_averted(df_age_behav, stratification_name = "Age, behaviour")
inc_area <- compute_infections_averted(df_area, stratification_name = "Area")
inc_age <- compute_infections_averted(df_age, stratification_name = "Age")
inc_behav <- compute_infections_averted(df_behav, stratification_name = "Behaviour")
inc_none <- compute_infections_averted(df_none, stratification_name = "None")

inc <- bind_rows(
  inc_area_age_behav,
  inc_area_age,
  inc_area_behav,
  inc_age_behav,
  inc_area,
  inc_age,
  inc_behav,
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
    infections_averted_cumulative_improvement = infections_averted_cumulative - infections_averted_cumulative_baseline,
    stratification = factor(
      stratification,
      levels = c("Area, age, behaviour", "Area, age", "Area, behaviour", "Age, behaviour", "Area", "Age", "Behaviour", "None")
    )
  )

pdf("infections-averted.pdf", h = 5, w = 6.25)

ggplot(inc, aes(x = population_cumulative, y = infections_averted_cumulative_improvement, col = stratification)) +
  geom_line(alpha = 0.8, size = 0.7) +
  scale_color_manual(values = multi.utils::cbpalette()) +
  scale_y_continuous(labels = label_number(scale = 1e-3)) +
  scale_x_continuous(labels = label_number(scale = 1e-6)) +
  labs(
    x = "Total population reached (10E6)",
    y = "Additional infections reached (10E3)",
    col = "Risk stratification"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
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
  scale_color_manual(values = multi.utils::cbpalette()[-c(4, 5)]) +
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

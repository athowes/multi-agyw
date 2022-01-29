#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("analyze_infections-averted")
# setwd("src/analyze_infections-averted")

df <- read_csv("depends/incidence-district-sexbehav.csv")

#' Assume some value for how much a hypothetical intervention reduces incidence by (multiplicatively)
p <- 0.5

df_area_age_behav <- df %>%
  select(iso3, area_id, age_group, starts_with("population_sex"), starts_with("incidence_sex")) %>%
  pivot_longer(
    cols = starts_with("population") | starts_with("incidence"),
    names_to = c("indicator", "category"),
    names_sep = "_"
  ) %>%
  pivot_wider(
    id_cols = c("area_id", "age_group", "indicator", "category"),
    names_from = "indicator",
    values_from = "value"
  )

df_area_age <- df_area_age_behav %>%
  group_by(area_id, age_group) %>%
  summarise(
    incidence = sum(incidence * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
    population = sum(population, na.rm = TRUE)
  )

df_area_behav <- df_area_age_behav %>%
  group_by(area_id, category) %>%
  summarise(
    incidence = sum(incidence * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
    population = sum(population, na.rm = TRUE)
  )

df_age_behav <- df_area_age_behav %>%
  group_by(age_group, category) %>%
  summarise(
    incidence = sum(incidence * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
    population = sum(population, na.rm = TRUE)
  )

compute_infections_averted <- function(df, stratification_name) {
  df %>%
    mutate(infections_averted = incidence * p * population) %>%
    #' Best opportunities have the highest incidence, take those first
    arrange(desc(incidence)) %>%
    mutate(
      population_cumulative = cumsum(population),
      infections_averted_cumulative = cumsum(infections_averted),
      stratification = stratification_name
    )
}

inc_area_age_behav <- compute_infections_averted(df_area_age_behav, stratification_name = "area_age_behav")
inc_area_age <- compute_infections_averted(df_area_age, stratification_name = "area_age")
inc_area_behav <- compute_infections_averted(df_area_behav, stratification_name = "area_behav")
inc_age_behav <- compute_infections_averted(df_age_behav, stratification_name = "age_behav")

pdf("infections-averted.pdf", h = 4, w = 6.25)

ggplot(inc_area_age_behav, aes(x = population_cumulative, y = infections_averted_cumulative)) +
  geom_line() +
  theme_minimal() +
  labs(x = "Total population reached",
       y = "Total infections averted",
       title = "Targetting based on age, district, and behaviour",
       subtitle = "Assume cost to reach each individual is equal, and incidence is reduced\nby 50% by the intervention")

dev.off()

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
    id_cols = c("area_id", "age_group", "indicator", "category"),
    names_from = "indicator",
    values_from = "value"
  )

df_area_age <- df_area_age_behav %>%
  group_by(area_id, age_group) %>%
  summarise(
    incidence = sum(incidence * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
    population = sum(population, na.rm = TRUE)
  ) %>%
  ungroup()

df_area_behav <- df_area_age_behav %>%
  group_by(area_id, category) %>%
  summarise(
    incidence = sum(incidence * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
    population = sum(population, na.rm = TRUE)
  ) %>%
  ungroup()

df_age_behav <- df_area_age_behav %>%
  group_by(age_group, category) %>%
  summarise(
    incidence = sum(incidence * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
    population = sum(population, na.rm = TRUE)
  ) %>%
  ungroup()

compute_infections_averted <- function(df, stratification_name) {
  df %>%
    mutate(infections_averted = incidence * p * population) %>%
    #' Best opportunities have the highest incidence, take those first
    arrange(desc(incidence)) %>%
    mutate(
      population_cumulative = cumsum(population),
      infections_averted_cumulative = cumsum(infections_averted),
      stratification = stratification_name
    ) %>%
    bind_rows(
      data.frame(population_cumulative = 0, infections_averted_cumulative = 0, stratification = stratification_name)
    )
}

inc_area_age_behav <- compute_infections_averted(df_area_age_behav, stratification_name = "Area, age, behaviour")
inc_area_age <- compute_infections_averted(df_area_age, stratification_name = "Area, age")
inc_area_behav <- compute_infections_averted(df_area_behav, stratification_name = "Area, behaviour")
inc_age_behav <- compute_infections_averted(df_age_behav, stratification_name = "Age, behaviour")

inc <- bind_rows(
  inc_area_age_behav,
  inc_area_age,
  inc_area_behav,
  inc_age_behav
)

pdf("infections-averted.pdf", h = 4, w = 6.25)

cbpalette <- c("#56B4E9","#009E73", "#E69F00", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")

ggplot(inc, aes(x = population_cumulative, y = infections_averted_cumulative, col = stratification)) +
  geom_line(linetype = 2) +
  theme_minimal() +
  scale_color_manual(values = cbpalette[-c(4, 5)]) +
  scale_y_continuous(labels = unit_format(unit = "T", scale = 1e-3)) +
  scale_x_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  labs(
    x = "Total population reached",
    y = "Total infections reached",
    title = "Population stratification targetting comparison",
    col = "Stratification"
  )

dev.off()

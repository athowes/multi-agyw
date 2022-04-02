#' https://tidyeval.tidyverse.org/dplyr.html
compute_infections_reached <- function(df, stratification_name, ...) {
  #' Aggreate incidence and population according to stratification
  if(stratification_name != "Area, age, behaviour") {
    df <- df %>%
      group_by(...) %>%
      summarise(
        incidence = sum(incidence * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
        population = sum(population, na.rm = TRUE)
      ) %>%
      ungroup()
  }

  if("area_id" %in% names(df)) {
    df <- mutate(df, iso3 = substr(area_id, 1, 3))
  }

  #' Add infections averted column
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

infections_reached_all_stratifications <- function(df) {
  inf_area_age_behav <- compute_infections_reached(df, stratification_name = "Area, age, behaviour")
  inf_area_age <- compute_infections_reached(df, stratification_name = "Area, age", area_id, age_group)
  inf_area_behav <- compute_infections_reached(df, stratification_name = "Area, behaviour", area_id, category)
  inf_age_behav <- compute_infections_reached(df, stratification_name = "Age, behaviour", age_group, category)
  inf_area <- compute_infections_reached(df, stratification_name = "Area", area_id)
  inf_age <- compute_infections_reached(df, stratification_name = "Age", age_group)
  inf_behav <- compute_infections_reached(df, stratification_name = "Behaviour", category)
  inf_none <- compute_infections_reached(df, stratification_name = "Baseline")

  inf <- bind_rows(
    inf_area_age_behav,
    inf_area_age,
    inf_area_behav,
    inf_age_behav,
    inf_area,
    inf_age,
    inf_behav,
    inf_none
  )

  #' Add "improvement over no targetting" column
  infections_averted_per_population_baseline <- inf %>%
    filter(stratification == "Baseline") %>%
    #' Just the end of the line
    filter(population_cumulative == max(population_cumulative)) %>%
    mutate(infections_averted_per_population_baseline = infections_averted_cumulative / population_cumulative) %>%
    pull(infections_averted_per_population_baseline)

  inf <- inf %>%
    mutate(
      infections_averted_cumulative_baseline = population_cumulative * infections_averted_per_population_baseline,
      infections_averted_cumulative_improvement = infections_averted_cumulative - infections_averted_cumulative_baseline,
      prop_infections_averted_baseline = infections_averted_cumulative_baseline / max(infections_averted_cumulative_baseline, na.rm = TRUE),
      prop_infections_averted_cumulative = infections_averted_cumulative / max(infections_averted_cumulative, na.rm = TRUE),
      prop_infections_averted_cumulative_improvement = prop_infections_averted_cumulative - prop_infections_averted_baseline,
      prop_population_cumulative = population_cumulative / max(population_cumulative, na.rm = TRUE),
      stratification = factor(
        stratification,
        levels = c("Area, age, behaviour", "Area, age", "Area, behaviour", "Age, behaviour", "Area", "Age", "Behaviour", "Baseline")
      )
    )

  return(inf)
}

plot_infections_reached_above_baseline <- function(df) {
  ggplot(df, aes(x = prop_population_cumulative, y = prop_infections_averted_cumulative_improvement, col = stratification)) +
    geom_line(alpha = 0.8, size = 1) +
    scale_color_manual(values = multi.utils::cbpalette()) +
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::label_percent(accuracy = 1L)) +
    # coord_fixed(ratio = 1) +
    labs(
      x = paste0("Percent of at risk population (", signif(max(df$population_cumulative, na.rm = TRUE), 3) / 10^6, "M) reached"),
      y = paste0("Percent of new infections (", signif(max(df$infections_averted_cumulative, na.rm = TRUE), 2) / 10^6, "M)\nreached beyond baseline"),
      col = "Risk stratification"
    ) +
    theme_minimal() +
    theme(
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 9)
    )
}

plot_infections_reached <- function(df) {
  ggplot(df, aes(x = prop_population_cumulative, y = prop_infections_averted_cumulative, col = stratification)) +
    geom_line(alpha = 0.8, size = 1) +
    scale_color_manual(values = multi.utils::cbpalette()) +
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::label_percent(accuracy = 1L)) +
    coord_fixed(ratio = 1) +
    labs(
      x = paste0("Percent of at risk population (", signif(max(df$population_cumulative, na.rm = TRUE), 3) / 10^6, "M) reached"),
      y = paste0("Percent of new infections\n(", signif(max(df$infections_averted_cumulative, na.rm = TRUE), 2) / 10^6, "M) reached"),
      col = ""
    ) +
    # guides(col = guide_legend(ncol = 2)) +
    theme_minimal() +
    theme(
      # legend.position = c(0.75, 0.3),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 9)
    )
}

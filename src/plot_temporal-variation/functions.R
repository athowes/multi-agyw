plot_temporal_interpolation <- function(df) {
  df %>%
    pivot_longer(
      cols = c(starts_with("estimate")),
      names_to = c(".value", "source"),
      names_pattern = "(.*)\\_(.*)"
    ) %>%
    group_by(survey_id, iso3, indicator, age_group, source) %>%
    summarise(
      estimate = mean(estimate, na.rm = TRUE)
    ) %>%
    #' Assuming the survey_id is structured as ISO2000DHS
    mutate(
      year = as.integer(substr(survey_id, 4, 7)),
      source = fct_recode(source, "Raw" = "raw", "Smoothed" = "smoothed")
    ) %>%
    ggplot(aes(x = year, y = estimate, col = source)) +
    geom_point(alpha = 0.5) +
    facet_grid(age_group ~ indicator, labeller = labeller(indicator = label_wrap_gen(20))) +
    labs(x = "Year of survey", y = "Estimate", col = "Country") +
    lims(x = c(2000L, 2018L)) +
    scale_color_manual(values = multi.utils::cbpalette()) +
    theme_minimal() +
    theme(
      strip.text = element_text(face = "bold"),
      legend.position = "bottom",
      legend.key.width = unit(4, "lines"),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
    )
}

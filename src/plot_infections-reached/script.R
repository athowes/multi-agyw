#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("plot_infections-reached")
# setwd("src/plot_infections-reached")

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

inf <- infections_reached_all_stratifications(df_area_age_behav)

plot_infections_reached_above_baseline <- function(df) {
  ggplot(df, aes(x = prop_population_cumulative, y = prop_infections_averted_cumulative_improvement, col = stratification)) +
    geom_line(alpha = 0.8, size = 1) +
    scale_color_manual(values = multi.utils::cbpalette()) +
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::label_percent(accuracy = 1L)) +
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

#' Infections reached above baseline
pdf("infections-reached-above-baseline.pdf", h = 3, w = 6.25)

plot_infections_reached_above_baseline(inf)

dev.off()

#' Save version for poster
ggsave(
  "infections-reached-above-baseline.png",
  plot_infections_reached_above_baseline(inf) + theme(legend.position = "bottom"),
  width = 6, height = 4, units = "in", dpi = 300
)

plot_infections_reached <- function(df) {
  ggplot(df, aes(x = prop_population_cumulative, y = prop_infections_averted_cumulative, col = stratification)) +
    geom_line(alpha = 0.8, size = 1) +
    scale_color_manual(values = multi.utils::cbpalette()) +
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::label_percent(accuracy = 1L)) +
    labs(
      x = paste0("Percent of at risk population (", signif(max(df$population_cumulative, na.rm = TRUE), 3) / 10^6, "M) reached"),
      y = paste0("Percent of new infections\n(", signif(max(df$infections_averted_cumulative, na.rm = TRUE), 2) / 10^6, "M) reached"),
      col = ""
    ) +
    guides(col = guide_legend(ncol = 2)) +
    theme_minimal() +
    theme(
      legend.position = c(0.75, 0.28),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 9)
    )
}

#' Infections reached
pdf("infections-reached.pdf", h = 3, w = 6.25)

plot_infections_reached(inf)

dev.off()

#' Save version for poster
ggsave(
  "infections-reached.png",
  plot_infections_reached(inf),
  width = 6, height = 4, units = "in", dpi = 300
)

#' Separate analysis for each country

inf_country <- df_area_age_behav %>%
  split(.$iso3) %>%
  lapply(function(x)
    infections_reached_all_stratifications(x) %>%
      mutate(iso3 = x$iso3[1])
  )

pdf("infections-reached-above-baseline-country.pdf", h = 3, w = 6.25)

lapply(inf_country, function(x)
  x %>%
    plot_infections_reached_above_baseline +
    labs(title = paste0(x$iso3[1]))
)

dev.off()

pdf("infections-reached-country.pdf", h = 3, w = 6.25)

lapply(inf_country, function(x)
  x %>%
    plot_infections_reached +
    labs(title = paste0(x$iso3[1]))
)

dev.off()

#' Quantification of points discussed

#' What is the minimum proportion of the population to reach in order to
#' achieve reaching over q \in [0, 1] proportion of the new infections?
min_pop_reached <- function(q) {
  inf %>%
    filter(prop_infections_averted_cumulative > q) %>%
    group_by(stratification) %>%
    filter(prop_population_cumulative == min(prop_population_cumulative)) %>%
    select(stratification, prop_infections_averted_cumulative, prop_population_cumulative)
}

min_pop_reached(0.25)
min_pop_reached(0.5)

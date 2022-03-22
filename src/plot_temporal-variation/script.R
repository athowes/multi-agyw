#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("plot_temporal-variation")
# setwd("src/plot_temporal-variation")

df_3p1 <- read_csv("depends/best-3p1-multi-sexbehav-sae.csv") %>%
  multi.utils::update_naming()

df_3p1_aaa <- read_csv("depends/best-3p1-aaa-multi-sexbehav-sae.csv") %>%
  multi.utils::update_naming()

pdf("3p1-temporal-interpolation.pdf", h = 4, w = 6.25)

df_3p1 %>%
  split(.$iso3) %>%
  lapply(function(x)
    plot_temporal_interpolation(x) +
      labs(title = paste0(x$iso3[1]))
  )

dev.off()

pdf("3p1-aaa-temporal-interpolation.pdf", h = 4, w = 6.25)

df_3p1_aaa %>%
  split(.$iso3) %>%
  lapply(function(x)
    plot_temporal_interpolation(x) +
      labs(title = paste0(x$iso3[1]))
  )

dev.off()

pdf("3p1-temporal-interpolation-ribbon.pdf", h = 4, w = 6.25)

df_3p1_ribbon <- df_3p1 %>%
  group_by(indicator, iso3, year, age_group) %>%
  summarise(
    mean_smoothed = mean(estimate_smoothed, na.rm = TRUE),
    lower_smoothed = quantile(estimate_smoothed, probs = 0.05, na.rm = TRUE),
    upper_smoothed = quantile(estimate_smoothed, probs = 0.95, na.rm = TRUE)
  )

df_3p1_raw <- df_3p1 %>%
  filter(!is.na(estimate_raw)) %>%
  group_by(indicator, survey_id, iso3, year, age_group) %>%
  summarise(mean_raw = mean(estimate_raw)) %>%
  mutate(type = substr(survey_id, 8, 11))

# scales::show_pal(multi.utils::cbpalette())
match_available_surveys_plot_palette <- c("DHS" = "#56B4E9", "PHIA" = "#009E73", "AIS" = "#E69F00", "BAIS" = "#CC79A7")

df_3p1_ribbon %>%
  split(.$iso3) %>%
  lapply(function(x)
    ggplot(x, aes(x = year, y = mean_smoothed)) +
      geom_ribbon(aes(ymin = lower_smoothed, ymax = upper_smoothed), alpha = 0.5) +
      geom_line() +
      geom_point(
        data = filter(df_3p1_raw, iso3 == x$iso3[1]),
        aes(x = year, y = mean_raw, col = type)
      ) +
      facet_grid(age_group ~ indicator) +
      scale_color_manual(values = match_available_surveys_plot_palette) +
      lims(x = c(2000L, 2018L)) +
      labs(title = paste0(x$iso3[1]), x = "Year", y = "Estimate", col = "Type") +
      theme_minimal() +
      theme(
        strip.text = element_text(face = "bold"),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
      )
  )

dev.off()

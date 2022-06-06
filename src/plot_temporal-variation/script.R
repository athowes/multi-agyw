#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("plot_temporal-variation")
# setwd("src/plot_temporal-variation")

df_3p1 <- read_csv("depends/best-3p1-multi-sexbehav-sae.csv") %>%
  filter(indicator != "sexnonregplus") %>%
  multi.utils::update_naming()

pdf("temporal-interpolation.pdf", h = 4, w = 6.25)

df_3p1 %>%
  split(.$iso3) %>%
  lapply(function(x)
    plot_temporal_interpolation(x) +
      labs(title = paste0(x$iso3[1]))
  )

dev.off()

pdf("temporal-interpolation-ribbon.pdf", h = 3.5, w = 6.25)

df_3p1_ribbon <- df_3p1 %>%
  group_by(indicator, iso3, year, age_group) %>%
  summarise(
    mean_smoothed = mean(estimate_smoothed, na.rm = TRUE),
    lower_smoothed = quantile(estimate_smoothed, probs = 0.05, na.rm = TRUE),
    upper_smoothed = quantile(estimate_smoothed, probs = 0.95, na.rm = TRUE)
  )

unique_surveys <- df_3p1 %>%
  pull(survey_id) %>%
  unique()

df_3p1_raw <- df_3p1 %>%
  filter(paste0(substr(area_id, 1, 3), year) %in% substr(unique_surveys, 1, 7)) %>%
  group_by(indicator, survey_id, iso3, year, age_group) %>%
  summarise(
    mean_raw = mean(estimate_raw, na.rm = TRUE),
    mean_part_raw = mean(estimate_part_raw, na.rm = TRUE)
  ) %>%
  mutate(
    type = substr(survey_id, 8, 11),
    #' Replace any NA instance of mean_raw with mean_part_raw and add
    #' an indicator as to if the repalcement happened
    raw_replaced = is.na(mean_raw),
    mean_raw = ifelse(raw_replaced, mean_part_raw, mean_raw)
  ) %>%
  mutate(
    raw_replaced = case_when(
      raw_replaced ~ "Partially direct",
      !raw_replaced ~ "Direct",
    )
  )

# scales::show_pal(multi.utils::cbpalette())
match_available_surveys_plot_palette <- c("DHS" = "#56B4E9", "PHIA" = "#009E73", "AIS" = "#E69F00", "BAIS" = "#CC79A7")

plotsA <- df_3p1_ribbon %>%
  split(.$iso3) %>%
  lapply(function(x)
    ggplot(x, aes(x = year, y = mean_smoothed)) +
      geom_ribbon(aes(ymin = lower_smoothed, ymax = upper_smoothed), alpha = 0.5) +
      geom_line() +
      geom_point(
        data = filter(df_3p1_raw, iso3 == x$iso3[1]),
        aes(x = year, y = mean_raw, col = type, shape = raw_replaced)
      ) +
      facet_grid(age_group ~ indicator) +
      scale_color_manual(values = match_available_surveys_plot_palette) +
      scale_shape_manual(values = c(19, 1)) +
      lims(x = c(2000L, 2018L)) +
      labs(title = paste0(x$iso3[1]), x = "Year", y = "Estimate", col = "Survey type", shape = "Estimate type") +
      theme_minimal() +
      theme(
        strip.text = element_text(face = "bold"),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 9),
        legend.spacing.x = unit(0.1, "cm")
      )
  )

plotsA

dev.off()

#' Sadly multi-page .png don't exist
#' This is a bit clunky but unsure if there is a better option
# lapply(1:length(plotsA), function(i) {
#   ggsave(
#     paste0("temporal-interpolation-ribbon-", i, ".png"),
#     plotsA[[i]],
#     width = 6.25, height = 3.5, units = "in", dpi = 300
#   )
# })

#' Checking estimates for one country (UGA)
pdf("uga-check.pdf", h = 7, w = 6.25)

df_3p1 %>%
  filter(iso3 == "Uganda", indicator == "Cohabiting partner") %>%
  select(year, age_group, estimate_raw, estimate_smoothed) %>%
  pivot_longer(
    cols = starts_with("estimate"),
    names_to = "type",
    values_to = "estimate"
  ) %>%
  ggplot(aes(x = age_group, y = estimate, col = type)) +
    geom_jitter() +
    facet_wrap(~year) +
    theme_minimal()

dev.off()

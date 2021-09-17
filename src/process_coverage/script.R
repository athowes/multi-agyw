#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("process_coverage")
# setwd("src/process_coverage")

df <- read_csv("depends/every-all-dhs-multinomial-smoothed-district-sexbehav.csv")

df <- df %>%
  filter(
    age_group != "Y015_024",
    model == "Model 6"
  ) %>%
  mutate(
    age_group = fct_recode(age_group,
      "15-19" = "Y015_019",
      "20-24" = "Y020_024",
      "25-29" = "Y025_029"
    ),
    indicator = fct_recode(indicator,
      "No sex (past 12 months)" = "nosex12m",
      "Cohabiting partner" = "sexcohab",
      "Nonregular partner(s)" = "sexnonregplus"
    )
  )

empirical_coverage <- function(x, nominal_coverage) {
  alpha <- (1 - nominal_coverage)
  y <- (x >= (alpha / 2)) & (x <= 1 - (alpha / 2))
  sum(y) / length(y)
}

nominal_df <- df %>%
  select(indicator, quantile) %>%
  filter(!is.na(quantile)) %>%
  split(.$indicator) %>%
  lapply(function(y) {
    empirical_coverage <- purrr::map_dbl(seq(0, 1, by = 0.01), ~ empirical_coverage(y$quantile, .x))
    data.frame(nominal_coverage = seq(0, 1, by = 0.01), empirical_coverage = empirical_coverage)
  }) %>%
  purrr::map_df(~as.data.frame(.x), .id = "indicator")

pdf("coverage.pdf", h = 4, w = 6.25)

histograms <- ggplot(df, aes(x = quantile)) +
  facet_grid(~indicator, drop = TRUE, scales = "free") +
  geom_histogram(aes(y=(..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]),
                 bins = 10, fill = "#D3D3D3", col = "#FFFFFF", alpha = 0.9) +
  geom_hline(linetype = "dashed", yintercept = 0.1, col = "#56B4E9") +
  labs(x = "Quantile", y = "Proportion in quantile") +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c(0, 0.25, 0.5, 0.75, 1))

nominal_empirical <- nominal_df %>%
  ggplot(aes(x = nominal_coverage, y = empirical_coverage)) +
  facet_grid(~factor(indicator, levels = c("No sex (past 12 months)", "Cohabiting partner", "Nonregular partner(s)")),
             drop = TRUE, scales = "free") +
  geom_line(col = "#D3D3D3") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", col = "#009E73") +
  ylim(0, 1) +
  labs(x = "Nominal coverage", y = "Empirical coverage") +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c(0, 0.25, 0.5, 0.75, 1)) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )

cowplot::plot_grid(histograms, nominal_empirical, ncol = 1)

dev.off()

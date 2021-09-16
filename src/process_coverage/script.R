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

pdf("coverage-histograms.pdf", h = 3.5, w = 6.25)

ggplot(df, aes(x = quantile)) +
  facet_grid(~indicator, drop = TRUE, scales = "free") +
  geom_histogram(aes(y=(..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]),
                 bins = 10, fill = "#D3D3D3", col = "#FFFFFF", alpha = 0.9) +
  geom_hline(linetype = "dashed", yintercept = 0.1, col = "#000000") +
  labs(x = "Quantile", y = "Proportion of raw estimates in quantile") +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c(0, 0.25, 0.5, 0.75, 1))

dev.off()

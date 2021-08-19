#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("plot_temporal-variation")
# setwd("src/plot_temporal-variation")

df <- read_csv("depends/every-all-dhs-multinomial-smoothed-district-sexbehav.csv")

df <- df %>%
  mutate(
    #' Assuming the survey_id is structured as ISO2000DHS
    year = substr(survey_id, 4, 7),
    #' Labels for plot
    age_group = fct_relevel(age_group, "Y015_024", after = 3) %>%
      fct_recode(
        "15-19" = "Y015_019",
        "20-24" = "Y020_024",
        "25-29" = "Y025_029",
        "15-24" = "Y015_024"
      ),
    indicator =
      fct_recode(indicator,
        "No sex (past 12 months)" = "nosex12m",
        "Cohabiting partner" = "sexcohab",
        "Nonregular partner(s) or paid\n for sex (past 12 months)" = "sexnonregplus"
      )
  ) %>%
  filter(model == "Model 9", age_group != "15-24")

pdf("temporal-variation.pdf", h = 10, w = 12)

ggplot(df, aes(x = year, y = estimate_smoothed, col = iso3, group = area_name)) +
  geom_line(alpha = 0.2) +
  facet_grid(age_group ~  indicator) +
  labs(x = "Year of survey", y = "Posterior mean proportion by region", col = "Country") +
  guides(col = guide_legend(override.aes = list(alpha = 0.5))) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.key.width = unit(4, "lines"),
  )

dev.off()

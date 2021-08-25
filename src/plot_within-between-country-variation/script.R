#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("plot_within-between-country-variation")
# setwd("src/plot_within-between-country-variation")

df <- read_csv("depends/every-all-dhs-multinomial-smoothed-district-sexbehav.csv")

df <- df %>%
  mutate(
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
  filter(
    model == "Model 9",
    age_group != "15-24",
  )

df_subnational <- df %>%
  filter(!(area_id %in% c("CMR", "KEN", "LSO", "MOZ", "MWI", "NAM", "SWZ", "TZA", "UGA", "ZAF", "ZMB", "ZWE")))

df_national <- setdiff(df, df_subnational)

pdf("within-between-country-variation.pdf", h = 10, w = 12)

ggplot(df_subnational, aes(x = survey_id, y = estimate_smoothed, col = iso3)) +
  geom_jitter(width = 0.1, alpha = 0.3, shape = 20) +
  geom_point(data = df_national, aes(x = survey_id, y = estimate_smoothed),
             shape = 21, fill = "white", col = "black", alpha = 0.7) +
  facet_grid(age_group ~  indicator) +
  coord_flip() +
  labs(x = "Country", y = "Posterior mean proportion by region") +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.key.width = unit(4, "lines"),
  )

dev.off()

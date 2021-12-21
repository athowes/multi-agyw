#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("plot_temporal-variation")
# setwd("src/plot_temporal-variation")

df <- read_csv("depends/best-3-multinomial-smoothed-district-sexbehav.csv")

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
  filter(
    age_group != "15-24"
  )

df_subnational <- df %>%
  filter(!(area_id %in% c("BWA", "CMR", "KEN", "LSO", "MOZ", "MWI", "NAM", "SWZ", "TZA", "UGA", "ZAF", "ZMB", "ZWE")))

df_national <- setdiff(df, df_subnational)

pdf("temporal-variation.pdf", h = 8.25, w = 11.75)

ggplot(df_subnational, aes(x = year, y = estimate_smoothed, col = iso3, group = area_name)) +
  geom_line(alpha = 0.1) +
  geom_line(data = df_national, aes(x = year, y = estimate_smoothed, col = iso3, group = area_name),
            size = 1.5) +
  facet_grid(age_group ~  indicator) +
  labs(x = "Year of survey", y = "Posterior mean proportion by region", col = "Country") +
  guides(col = guide_legend(override.aes = list(alpha = 0.5))) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.key.width = unit(4, "lines"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

dev.off()

#' Add columns to df_national for the quantiles of the subnational estimates
df_national <- df_national %>%
  left_join(
    df_subnational %>%
      group_by(iso3, age_group, indicator) %>%
      summarise(
        subnational_q975 = quantile(estimate_smoothed, probs = 0.975),
        subnational_q750 = quantile(estimate_smoothed, probs = 0.750),
        subnational_q250 = quantile(estimate_smoothed, probs = 0.250),
        subnational_q025 = quantile(estimate_smoothed, probs = 0.025)
      ),
    by = c("iso3", "age_group", "indicator")
  )

pdf("temporal-variation-alt.pdf", h = 8.25, w = 11.75)

ggplot(df_national, aes(x = year, y = estimate_smoothed, fill = iso3, group = area_name)) +
  geom_line(aes(col = iso3), size = 1.5) +
  geom_ribbon(aes(ymin = subnational_q025, ymax = subnational_q975), alpha = 0.1) +
  geom_ribbon(aes(ymin = subnational_q250, ymax = subnational_q750), alpha = 0.3) +
  facet_grid(age_group ~  indicator) +
  labs(x = "Year of survey", y = "Posterior mean proportion", col = "Country", fill = "Country") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.key.width = unit(4, "lines"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

dev.off()

#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("plot_within-between-country-variation")
# setwd("src/plot_within-between-country-variation")

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
        "Nonregular partner(s)" = "sexnonregplus"
      ),
    iso3 = fct_recode(iso3,
      "Cameroon" = "CMR",
      "Kenya" = "KEN",
      "Lesotho" = "LSO",
      "Mozambique" = "MOZ",
      "Malawi" = "MWI",
      "Uganda" = "UGA",
      "Zambia" = "ZMB",
      "Zimbabwe" = "ZWE",
    )
  ) %>%
  filter(
    model == "Model 6",
    age_group != "15-24",
  ) %>%
  #' Only the most recent survey in each year
  group_by(iso3) %>%
  filter(year == max(year)) %>%
  ungroup()

df_subnational <- df %>%
  filter(!(area_id %in% c("CMR", "KEN", "LSO",
                          "MOZ", "MWI", "NAM",
                          "SWZ", "TZA", "UGA",
                          "ZAF", "ZMB", "ZWE")))

df_national <- setdiff(df, df_subnational)

pdf("within-between-country-variation.pdf", h = 7, w = 6.25)

cbpalette <- c("#56B4E9","#009E73", "#E69F00", "#F0E442","#0072B2","#D55E00","#CC79A7", "#999999")

ggplot(df_subnational, aes(x = iso3, y = estimate_smoothed, col = iso3)) +
  geom_jitter(width = 0.1, alpha = 0.6, shape = 20) +
  geom_point(data = df_national, aes(x = iso3, y = estimate_smoothed),
             shape = 21, size = 2, fill = "white", col = "black", alpha = 0.9) +
  facet_grid(age_group ~  indicator) +
  scale_color_manual(values = cbpalette) +
  scale_y_continuous(labels = function(x) paste0(100 * x, "%")) +
  coord_flip() +
  labs(x = "", y = "Proportion") +
  theme_minimal() +
  theme(
    panel.spacing = unit(1.5, "lines"),
    legend.position = "none"
  )

dev.off()

#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("plot_within-between-country-variation")
# setwd("src/plot_within-between-country-variation")

df <- read_csv("depends/every-all-dhs-multinomial-smoothed-district-sexbehav.csv")

#' When there is only one survey, we want to select Model 3, and when there are multiple, we want to select Model 6
single_survey <- df %>%
  group_by(iso3) %>%
  select(survey_id) %>%
  unique() %>%
  count() %>%
  filter(n == 1)

model_selector <- function(iso3, model) {
  case_when(
    iso3 %in% single_survey$iso3 ~ model == "Model 3",
    T ~ model == "Model 6"
  )
}

df <- df %>%
  filter(
    model_selector(iso3, model),
    age_group != "Y015_024",
  ) %>%
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
    iso3 =
      fct_recode(iso3,
        "Botswana" = "BWA",
        "Cameroon" = "CMR",
        "Kenya" = "KEN",
        "Lesotho" = "LSO",
        "Mozambique" = "MOZ",
        "Malawi" = "MWI",
        "Namibia" = "NAM",
        "Swaziland" = "SWZ",
        "Tanzania" = "TZA",
        "Uganda" = "UGA",
        "South Africa" = "ZAF",
        "Zambia" = "ZMB",
        "Zimbabwe" = "ZWE"
    )
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

#' Add region column
region_key <- c(
  "Botswana" = "South",
  "Cameroon" = "Central",
  "Kenya" = "East",
  "Lesotho" = "South",
  "Mozambique" = "South",
  "Malawi" = "South",
  "Namibia" = "South",
  "Swaziland" = "South",
  "Tanzania" = "East",
  "Uganda" = "East",
  "South Africa" = "South",
  "Zambia" = "South",
  "Zimbabwe" = "South"
) %>%
  as.data.frame() %>%
  rename("Region" = ".") %>%
  tibble::rownames_to_column("iso3")

df_subnational <- df_subnational %>%
  left_join(region_key, by = "iso3")

pdf("within-between-country-variation.pdf", h = 7, w = 6.25)

ggplot(df_subnational, aes(x = fct_rev(iso3), y = estimate_smoothed, col = Region)) +
  geom_jitter(width = 0.1, alpha = 0.6, shape = 20) +
  geom_point(data = df_national, aes(x = fct_rev(iso3), y = estimate_smoothed),
             shape = 21, size = 2, fill = "white", col = "black", alpha = 0.9) +
  facet_grid(age_group ~  indicator) +
  scale_color_manual(values = cbpalette) +
  scale_y_continuous(labels = function(x) paste0(100 * x, "%")) +
  coord_flip() +
  labs(x = "", y = "Proportion") +
  guides(colour = guide_legend(override.aes = list(alpha = 0.9))) +
  theme_minimal() +
  theme(
    panel.spacing = unit(1.5, "lines"),
    legend.position = "bottom",
  )

dev.off()

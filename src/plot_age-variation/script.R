#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("plot_age-variation")
# setwd("src/plot_age-variation")

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
    age_group != "15-24"
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

df_age_country <- df %>%
  group_by(iso3, age_group, indicator) %>%
  summarise(estimate_smoothed = mean(estimate_smoothed, na.rm = TRUE))

df_age <- df %>%
  group_by(age_group, indicator) %>%
  summarise(estimate_smoothed = mean(estimate_smoothed, na.rm = TRUE))

pdf("age-variation.pdf", h = 3.5, w = 6.25)

cbpalette <- c("#56B4E9","#009E73", "#E69F00", "#F0E442","#0072B2","#D55E00","#CC79A7", "#999999")

ggplot(df_age_country, aes(x = age_group, y = estimate_smoothed, group = indicator, col = indicator)) +
  geom_point(alpha = 0.4) +
  geom_line(data = df_age, aes(x = age_group, y = estimate_smoothed, group = indicator, col = indicator),
            size = 1.5, alpha = 0.7) +
  scale_color_manual(values = cbpalette) +
  theme_minimal() +
  scale_y_continuous(labels = function(x) paste0(100 * x, "%")) +
  labs(x = "Age group", y = "Proportion", col = "Category")

dev.off()

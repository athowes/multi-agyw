#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("check_dhs-phia")
# setwd("src/check_dhs-phia")

df <- read_csv("depends/survey_indicators_sexbehav.csv")

pdf("dhs-phia.pdf", h = 10, w = 6.25)

df %>%
  filter(indicator %in% c("nosex12m", "sexcohab", "sexnonregplus")) %>%
  group_by(indicator, survey_id) %>%
  summarise(estimate = mean(estimate)) %>%
  mutate(
    iso3 = substr(survey_id, 1, 3),
    year = as.numeric(substr(survey_id, 4, 7)),
    type = substr(survey_id, 8, 11)
  ) %>%
  ggplot(aes(x = year, y = estimate, col = type)) +
  geom_point() +
  facet_grid(iso3 ~ indicator)

dev.off()


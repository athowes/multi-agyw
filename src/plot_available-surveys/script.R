#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("plot_available-surveys")
# setwd("src/plot_available-surveys/")

iso3 <- c("BWA", "CMR", "KEN", "LSO", "MOZ", "MWI", "NAM", "SWZ", "TZA", "UGA", "ZAF", "ZMB", "ZWE")
files <- paste0("depends/", tolower(iso3), "_survey_indicators_sexbehav.csv")

df <- lapply(files, function(file) {
  read_csv(file) %>%
    filter(
      #' Just at the country level
      area_id == toupper(substr(file, 9, 11)),
      #' Suppose that this is a representative survey question
      indicator == "sexcohab"
    ) %>%
    group_by(survey_id) %>%
    summarise(sample_size = sum(n_observations))
}) %>%
  bind_rows()

df <- df %>%
  mutate(
    iso3 = substr(survey_id, 1, 3),
    country = fct_recode(iso3,
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
    ),
    year = substr(survey_id, 4, 7),
    type = substr(survey_id, 8, length(survey_id))
  )

write_csv(df, "available-surveys.csv", na = "")

pdf("available-surveys.pdf", h = 3, w = 6.25)

cbpalette <- c("#56B4E9", "#009E73", "#E69F00", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")

df %>%
  ggplot(aes(x = year, y = fct_rev(country), col = type)) +
  geom_point(size = 2) +
  labs(x = "", y = "", col = "Survey type") +
  scale_color_manual(values = cbpalette) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    plot.title = element_text(face = "bold"),
    legend.key.width = unit(4, "lines")
  )

dev.off()

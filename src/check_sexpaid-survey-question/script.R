#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("check_sexpaid-survey-question")
# setwd("src/check_sexpaid-survey-question/")

#' "UGA" gave me troubles so leaving it out for now
priority_countries <- list("CMR", "KEN", "LSO", "MOZ", "MWI", "NAM", "SWZ", "TZA", "ZAF", "ZMB", "ZWE")

#' Getting all the data is not an efficient way of doing this but...
survey_sexbehav <- lapply(priority_countries, function(iso3) {
  surveys <- naomi.utils::create_surveys_dhs(iso3)
  naomi.utils::create_sexbehav_dhs(surveys)
})

df <- survey_sexbehav %>%
  bind_rows() %>%
  mutate(iso3 = substr(survey_id, 1, 3),
         year = substr(survey_id, 4, 7),
         .after = survey_id) %>%
  group_by(iso3, year) %>%
  summarise(giftsvar = mean(giftsvar))

#' Save as useful for future models
write_csv(df, "sexpaid-survey-question.csv", na = "")

#' V791A indicates whether the woman had received (or given) money or gifts in exchange
#' for sexual intercourse in the past year. For surveys without V791A, V767A-C, describing
#' the most recent three partners are used instead.
pdf("sexpaid-survey-question.pdf", h = 3, w = 6.25)

df %>%
  mutate(
    iso3 = fct_recode(iso3,
      "Cameroon" = "CMR",
      "Kenya" = "KEN",
      "Lesotho" = "LSO",
      "Mozambique" = "MOZ",
      "Malawi" = "MWI",
      "Namibia" = "NAM",
      "Swaziland" = "SWZ",
      "Tanzania" = "TZA",
      "South Africa" = "ZAF",
      "Uganda" = "UGA",
      "Zambia" = "ZMB",
      "Zimbabwe" = "ZWE",
    )
  ) %>%
  ggplot(aes(x = year, y = fct_rev(iso3), col = ifelse(giftsvar, "Yes", "No"))) +
    geom_point(size = 2) +
    labs(x = "", y = "", col = "Includes V7191A?") +
    scale_color_manual(values = c("#D3D3D3", "#00855A")) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      plot.title = element_text(face = "bold"),
      legend.key.width = unit(4, "lines")
    )

dev.off()

#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("plot_available-surveys")
# setwd("src/plot_available-surveys/")

iso3 <- c("BWA", "CMR", "KEN", "LSO", "MOZ", "MWI", "NAM", "SWZ", "TZA", "UGA", "ZAF", "ZMB", "ZWE")
files <- paste0("depends/", tolower(iso3), "_survey_indicators_sexbehav.csv")

df <- lapply(files, function(file) {
  #' indicators as produced by fitting reports
  ind <- read_csv(file)

  #' What's the average value of giftsvar in each survey?
  giftsvar <- ind %>%
    filter(indicator == "giftsvar") %>%
    group_by(survey_id) %>%
    summarise(giftsvar = as.factor(mean(estimate)))

  ind %>%
    filter(
      #' Just at the country level
      area_id == toupper(substr(file, 9, 11)),
      #' Suppose that this is a representative survey question
      indicator == "sexcohab"
    ) %>%
    group_by(survey_id) %>%
    summarise(sample_size = sum(n_observations)) %>%
    left_join(giftsvar, by = "survey_id")
}) %>%
  bind_rows()

fct_case_when <- function(...) {
  args <- as.list(match.call())
  levels <- sapply(args[-1], function(f) f[[3]])
  levels <- levels[!is.na(levels)]
  factor(dplyr::case_when(...), levels = levels)
}

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
    year = as.numeric(substr(survey_id, 4, 7)),
    type = substr(survey_id, 8, length(survey_id)),
    sample_size_factor = fct_case_when(
      sample_size < 5000 ~ "<5k",
      sample_size < 10000 ~ "5k-10k",
      sample_size < 15000 ~ "10k-15k",
      sample_size < 20000 ~ "15k-20k",
      20000 <= sample_size ~ ">20k"
    )
  )

write_csv(df, "available-surveys.csv", na = "")

pdf("available-surveys.pdf", h = 4, w = 6.25)

cbpalette <- c("#56B4E9", "#009E73", "#E69F00", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")

df %>%
  #' For now replace NA with a string NA
  mutate(
    giftsvar = fct_case_when(
      giftsvar == 1 ~ "Yes",
      giftsvar == 0 ~ "No",
      is.na(giftsvar) ~ "Missing"
    )
  ) %>%
  ggplot(aes(x = year, y = fct_rev(country), col = type, size = sample_size_factor, shape = giftsvar)) +
  geom_point(position = ggstance::position_dodgev(height = 0.75)) +
  labs(x = "", y = "", col = "Type", shape = "Paid sex question?", size = "Sample size") +
  scale_color_manual(values = cbpalette[c(3, 7, 1, 2)]) +
  scale_x_continuous(breaks = min(df$year):max(df$year)) +
  scale_size_discrete(range = c(2, 5)) +
  scale_shape_manual(values = c(2, 1, 4)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.box = "vertical"
  )

dev.off()

df %>%
  select(country, type, year, sample_size) %>%
  mutate(sample_size = round(sample_size, -2)) %>%
  rename(
    "Country" = "country",
    "Type" = "type",
    "Year" = "year",
    "Sample size" = "sample_size"
  ) %>%
  gt() %>%
  cols_align(
    align = c("left")
  ) %>%
  as_latex() %>%
  as.character() %>%
  cat(file = "available-surveys.txt")

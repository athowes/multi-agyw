#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("plot_available-surveys")
# setwd("src/plot_available-surveys/")

priority_iso3 <- multi.utils:::priority_iso3()
files <- paste0("depends/", tolower(priority_iso3), "_survey_indicators_sexbehav.csv")

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
    group_by(survey_id, age_group) %>%
    summarise(sample_size = sum(n_observations)) %>%
    left_join(giftsvar, by = "survey_id")
}) %>%
  bind_rows() %>%
  ungroup()

fct_case_when <- function(...) {
  args <- as.list(match.call())
  levels <- sapply(args[-1], function(f) f[[3]])
  levels <- levels[!is.na(levels)]
  factor(dplyr::case_when(...), levels = levels)
}

df <- df %>%
  pivot_wider(
    names_from = age_group,
    values_from = sample_size
  ) %>%
  select(-Y015_024) %>%
  mutate(
    Y015_029 = Y015_019 + Y020_024 + Y025_029,
    iso3 = substr(survey_id, 1, 3),
    country = fct_recode(iso3,
      "Botswana" = "BWA",
      "Cameroon" = "CMR",
      "Kenya" = "KEN",
      "Lesotho" = "LSO",
      "Mozambique" = "MOZ",
      "Malawi" = "MWI",
      "Namibia" = "NAM",
      "Eswatini" = "SWZ",
      "Tanzania" = "TZA",
      "Uganda" = "UGA",
      "South Africa" = "ZAF",
      "Zambia" = "ZMB",
      "Zimbabwe" = "ZWE"
    ),
    year = as.numeric(substr(survey_id, 4, 7)),
    type = substr(survey_id, 8, 13),
    sample_size_factor = fct_case_when(
      Y015_029 < 2500 ~ "<2.5k",
      Y015_029 < 5000 ~ "2.5k-5k",
      Y015_029 < 10000 ~ "5k-10k",
      Y015_029 < 15000 ~ "10k-15k"
    )
  )

write_csv(df, "available-surveys.csv", na = "")

pdf("available-surveys.pdf", h = 3.5, w = 6.25)

plotA <- df %>%
  #' For now replace NA with a string NA
  mutate(
    giftsvar = fct_case_when(
      giftsvar == 1 ~ "Yes",
      giftsvar == 0 ~ "No",
      is.na(giftsvar) ~ "Missing"
    )
  ) %>%
  ggplot(aes(x = year, y = fct_rev(country), col = type, size = sample_size_factor, shape = giftsvar)) +
  geom_point(alpha = 0.7) +
  labs(x = "", y = "", col = "Survey type", shape = "Does the survey include a specific\nquestion about transactional sex?", size = "Sample size") +
  scale_color_manual(values = multi.utils:::cbpalette()[c(3, 7, 1, 2)]) +
  scale_x_continuous(breaks = min(df$year):max(df$year)) +
  scale_size_discrete(range = c(2, 5)) +
  scale_shape_manual(values = c(17, 19)) +
  guides(
    colour = guide_legend(ncol = 2, override.aes = list(size = 3), order = 1),
    size = guide_legend(ncol = 2, override.aes = list(shape = 19, col = "darkgrey"), order = 2),
    shape = guide_legend(ncol = 2, override.aes = list(size = 3, col = "darkgrey"), order = 3)
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    legend.box = "vertical",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 9)
  )

plotA

dev.off()

ggsave(
  "available-surveys.png",
  plotA,
  width = 6.25, height = 3.5, units = "in", dpi = 300
)

available_surveys_gt <- df %>%
  select(country, type, year, giftsvar, Y015_019, Y020_024, Y025_029, Y015_029) %>%
  mutate(
    giftsvar = case_when(
      giftsvar == 1 ~ "[/]cmark",
      giftsvar == 0 ~ "[/]xmark"
    )
  ) %>%
  rename(
    "Country" = "country",
    "Type" = "type",
    "Year" = "year",
    "Transactional question" = "giftsvar",
    "15-19" = "Y015_019",
    "20-24" = "Y020_024",
    "25-29" = "Y025_029",
    "Total" = "Y015_029",
  ) %>%
  gt(groupname_col = "Country") %>%
  tab_spanner(
    label = "Sample size",
    columns = c("15-19", "20-24", "25-29", "Total")
  ) %>%
  summary_rows(
    groups = TRUE,
    columns = c("15-19", "20-24", "25-29", "Total"),
    fns = list("Total" = ~sum(., na.rm = TRUE)),
    drop_trailing_zeros = TRUE,
    missing_text = "",
    sep_mark = ""
  ) %>%
  grand_summary_rows(
    columns = c("15-19", "20-24", "25-29", "Total"),
    fns = list("Total" = ~sum(., na.rm = TRUE)),
    drop_trailing_zeros = TRUE,
    missing_text = "",
    sep_mark = ""
  )

saveRDS(available_surveys_gt, "available-surveys.rds")

available_surveys_gt %>%
  as_latex() %>%
  as.character() %>%
  cat(file = "available-surveys.txt")

#' What was the raw FSW proportion in surveys with and without a specific transactional question?
giftsvar_surveys <- filter(df, giftsvar == 1) %>% pull(survey_id)

lapply(files, function(file) {
  read_csv(file)
}) %>%
  bind_rows() %>%
  mutate(giftsvar_survey = survey_id %in% giftsvar_surveys) %>%
  group_by(giftsvar_survey) %>%
  filter(indicator == "sexpaid12m") %>%
  summarise(
    mean = 100 * mean(estimate),
    median = 100 * median(estimate)
  )

#' The surveys that https://www.statcompiler.com/en/ thinks we should have
#' Does not include PHIA surveys
stat_compiler <- read_xlsx("STATcompilerExport2021127_12313.xlsx", range = "A4:E110") %>%
  select(country = Country, survey =  Survey)

#' iso3 codes from https://gist.github.com/tadast/8827699
country_codes <- read_csv("countries_codes_and_coordinates.csv") %>%
  select(country = Country, iso3 = `Alpha-3 code`)

stat_compiler <- stat_compiler %>%
  left_join(country_codes)  %>%
  filter(iso3 %in% priority_iso3) %>%
  separate(survey, c("year", "type"), " ")

#' How many surveys of each type?
types_stat_compiler <- stat_compiler %>%
  group_by(country, type) %>%
  summarise(count = n())

types_available_surveys <- df %>%
  select(country, year, type) %>%
  filter(!(type %in% c("BAIS", "PHIA"))) %>%
  group_by(country, type) %>%
  summarise(count = n())

disputed_countries <- setdiff(types_stat_compiler, types_available_surveys)$country
disputed_countries <- c(disputed_countries, setdiff(types_available_surveys, types_stat_compiler)$country)

sink("surveys-stat-compiler.txt")

#' Note on multiple years that Naomi uses the calendar quarter for the survey midpoint
cat("The surveys in the disputed countries which we have are:")
filter(df, country %in% disputed_countries)

cat("The surveys that STATcomplier thinks that we should have are:")
filter(stat_compiler, country %in% disputed_countries)

sink()

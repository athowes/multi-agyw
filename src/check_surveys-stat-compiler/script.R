#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("check_surveys-stat-compiler")
# setwd("src/surveys-stat-compiler/")

#' The surveys that https://www.statcompiler.com/en/ thinks we should have
#' Does not include PHIA surveys
stat_compiler <- read_xlsx("STATcompilerExport2021127_12313.xlsx", range = "A4:E110") %>%
  rename(country = Country,
         survey =  Survey) %>%
  select(country, survey) %>%
  mutate(country = ifelse(country == "Eswatini", "Swaziland", country))


#' iso3 codes from https://gist.github.com/tadast/8827699
country_codes <- read_csv("countries_codes_and_coordinates.csv") %>%
  select(Country, `Alpha-3 code`) %>%
  rename(country = Country,
         iso3 = `Alpha-3 code`)

priority_countries <- c("BWA", "CMR", "KEN", "LSO", "MOZ", "MWI", "NAM", "SWZ", "TZA", "UGA", "ZAF", "ZMB", "ZWE")

stat_compiler <- stat_compiler %>%
  left_join(country_codes)  %>%
  filter(iso3 %in% priority_countries) %>%
  separate(survey, c("year", "type"), " ")

#' The surveys we actually have
available_surveys <- read_csv("depends/available-surveys.csv") %>%
  select(country, year, type)

#' How many surveys of each type?
types_stat_compiler <- stat_compiler %>%
  group_by(country, type) %>%
  summarise(count = n())

types_available_surveys <- available_surveys %>%
  filter(!(type %in% c("BAIS", "PHIA"))) %>%
  group_by(country, type) %>%
  summarise(count = n())

disputed_countries <- setdiff(types_stat_compiler, types_available_surveys)$country
disputed_countries <- c(disputed_countries, setdiff(types_available_surveys, types_stat_compiler)$country)

sink("surveys-stat-compiler.txt")

cat("The surveys in the disputed countries which we have are:")
filter(available_surveys, country %in% disputed_countries)

cat("The surveys that STATcomplier thinks that we should have are:")
filter(stat_compiler, country %in% disputed_countries)

sink()

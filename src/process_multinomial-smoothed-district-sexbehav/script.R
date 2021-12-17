#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("process_multinomial-smoothed-district-sexbehav")
# setwd("src/process_multinomial-smoothed-district-sexbehav")

iso3 <- c("BWA", "CMR", "LSO", "MWI", "NAM", "TZA", "UGA", "ZAF", "ZMB", "ZWE")

#' The four category survey estimates
files <- paste0("depends/", tolower(iso3), "_4-multinomial-smoothed-district-sexbehav.csv")
df <- bind_rows(lapply(files, function(file) read_csv(file)))

write_csv(df, "every-4-multinomial-smoothed-district-sexbehav.csv", na = "")

#' Best four category models

df <- df %>%
  filter(
    model == "Model 3",
  )

write_csv(df, "best-4-multinomial-smoothed-district-sexbehav.csv", na = "")

iso3 <- c("BWA", "CMR", "KEN", "LSO", "MOZ", "MWI", "NAM", "SWZ", "TZA", "UGA", "ZAF", "ZMB", "ZWE")

#' The three category estimates
files <- paste0("depends/", tolower(iso3), "_3-multinomial-smoothed-district-sexbehav.csv")
df <- bind_rows(lapply(files, function(file) read_csv(file)))

write_csv(df, "every-3-multinomial-smoothed-district-sexbehav.csv", na = "")

#' Best three category models

#' When there is only one survey, we want to select Model 3, and when there are multiple, we want to select Model 6
single_survey <- df %>%
  group_by(iso3) %>%
  select(survey_id) %>%
  unique() %>%
  count() %>%
  filter(n == 1)

#' Is there a way to automate this based on model comparison output?
model_selector <- function(iso3, model) {
  case_when(
    iso3 %in% single_survey$iso3 ~ model == "Model 3",
    T ~ model == "Model 6"
  )
}

df <- df %>%
  filter(
    model_selector(iso3, model),
  )

write_csv(df, "best-3-multinomial-smoothed-district-sexbehav.csv", na = "")

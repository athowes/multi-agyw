#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("process_multi-sexbehav-sae")
# setwd("src/process_multi-sexbehav-sae")

iso3 <- c("BWA", "CMR", "LSO", "MWI", "NAM", "TZA", "UGA", "ZAF", "ZMB", "ZWE")

#' The four category survey estimates
files <- paste0("depends/", tolower(iso3), "_4-multi-sexbehav-sae.csv")
df <- bind_rows(lapply(files, function(file) read_csv(file)))

write_csv(df, "every-4-aaa-multi-sexbehav-sae.csv", na = "")

#' Best four category models
df <- filter(df, model == "Model 3")

write_csv(df, "best-4-aaa-multi-sexbehav-sae.csv", na = "")

priority_iso3 <- multi.utils::priority_iso3()

#' The three category estimates
files <- paste0("depends/", tolower(priority_iso3), "_3-multi-sexbehav-sae.csv")
df <- bind_rows(lapply(files, function(file) read_csv(file)))

write_csv(df, "every-3-aaa-multi-sexbehav-sae.csv", na = "")

#' Best three category models

#' When there is only one survey, we want to select Model 3, and when there are multiple, we want to select Model 6
single_survey <- df %>%
  group_by(iso3) %>%
  select(survey_id) %>%
  unique() %>%
  count() %>%
  filter(n == 1) %>%
  pull(iso3)

#' Is there a way to automate this based on model comparison output?
model_selector <- function(iso3, model) {
  case_when(
    iso3 %in% single_survey ~ model == "Model 3",
    T ~ model == "Model 6"
  )
}

df <- filter(df, model_selector(iso3, model))

write_csv(df, "best-3-aaa-multi-sexbehav-sae.csv", na = "")

#' Three category samples
files <- paste0("depends/", tolower(priority_iso3), "_3-multi-sexbehav-sae-samples.rds")
samples <- lapply(files, function(file) readRDS(file))

saveRDS(samples, "every-3-aaa-multi-sexbehav-sae-samples.rds")

#' TODO create best-3-aaa-multi-sexbehav-sae-samples.rds

#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("process_multinomial-smoothed-district-sexbehav")
# setwd("src/process_multinomial-smoothed-district-sexbehav")

iso3 <- c("BWA", "CMR", "KEN", "LSO", "MOZ", "MWI", "NAM", "SWZ", "TZA", "UGA", "ZAF", "ZMB", "ZWE")

#' The four category survey estimates
files <- paste0("depends/", tolower(iso3), "_4-multinomial-smoothed-district-sexbehav.csv")
df <- bind_rows(lapply(files, function(file) read_csv(file)))

write_csv(df, "every-4-multinomial-smoothed-district-sexbehav.csv", na = "")

iso3 <- c("BWA", "CMR", "KEN", "LSO", "MOZ", "MWI", "NAM", "SWZ", "TZA", "UGA", "ZAF", "ZMB", "ZWE")

#' The three category estimates
files <- paste0("depends/", tolower(iso3), "_3-multinomial-smoothed-district-sexbehav.csv")
df <- bind_rows(lapply(files, function(file) read_csv(file)))

write_csv(df, "every-3-multinomial-smoothed-district-sexbehav.csv", na = "")

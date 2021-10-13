#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("process_multinomial-smoothed-district-sexbehav")
# setwd("src/process_multinomial-smoothed-district-sexbehav")

iso3 <- c("BWA", "CMR", "KEN", "LSO", "MOZ", "MWI", "NAM", "SWZ", "TZA", "UGA", "ZAF", "ZMB", "ZWE")

#' The single survey four category estimates
files <- paste0("depends/", tolower(iso3), "_multinomial-smoothed-district-sexbehav.csv")
df <- bind_rows(lapply(files, function(file) read_csv(file)))

write_csv(df, "every-multinomial-smoothed-district-sexbehav.csv", na = "")

iso3 <- c("BWA", "CMR", "KEN", "LSO", "MOZ", "MWI", "NAM", "SWZ", "TZA", "UGA", "ZAF", "ZMB", "ZWE")

#' The multiple survey three category estimates
files <- paste0("depends/", tolower(iso3), "_all-dhs-multinomial-smoothed-district-sexbehav.csv")
df <- bind_rows(lapply(files, function(file) read_csv(file)))

write_csv(df, "every-all-dhs-multinomial-smoothed-district-sexbehav.csv", na = "")

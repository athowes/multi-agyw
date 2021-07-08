#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("process_information-criteria")
# setwd("src/process_information-criteria")

iso3 <- c("CMR", "KEN", "LSO", "MOZ", "MWI", "NAM", "SWZ", "TZA", "UGA", "ZAF", "ZMB", "ZWE")
files <- paste0("depends/", tolower(iso3), "_information-criteria.csv")

df <- bind_rows(lapply(files, function(file) read_csv(file)))

write_csv(df, "all-model-comparison.csv", na = "")

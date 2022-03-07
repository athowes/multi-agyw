source("make/utils.R")

#' Prepare PHIA data in the 9 GF AGYW countries that have surveys
iso3 <- c("CMR", "MWI", "LSO", "NAM", "SWZ", "TZA", "UGA", "ZMB", "ZWE")
reports <- paste0(tolower(iso3), "_survey_phia")
run_commit_push(reports)

#' Prepare BAIS data in Botswana
run_commit_push("bwa_survey_bais")

#' Prepare sexual behaviour datasets in the 13 GF AGYW countries minus BWA
iso3 <- c("BWA", "CMR", "KEN", "LSO", "MOZ", "MWI", "NAM", "SWZ", "TZA", "UGA", "ZAF", "ZMB", "ZWE")
reports <- paste0(tolower(iso3), "_survey_behav")
run_commit_push(reports)

#' Make a plot of all available surveys for manuscript
run_commit_push("plot_available-surveys")

#' Process all the data into one file
run_commit_push("process_all-data")

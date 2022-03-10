source("make/utils.R")

#' Prepare PHIA data in the 9 GF AGYW countries that have surveys
iso3 <- c("CMR", "LSO", "MWI", "NAM", "SWZ", "TZA", "UGA", "ZMB", "ZWE")
reports <- paste0(tolower(iso3), "_survey_phia")
run_commit_push(reports)

#' Prepare BAIS data in Botswana
run_commit_push("bwa_survey_bais")

#' Prepare sexual behaviour datasets in the 13 GF AGYW countries minus BWA
#' Cache (with .zip files) and download (DHS API) errors here!
#' Might try using tryCatch while loop instead
iso3 <- c("BWA", "CMR", "KEN", "LSO", "MOZ", "MWI", "NAM", "SWZ", "TZA", "UGA", "ZAF", "ZMB", "ZWE")
reports <- paste0(tolower(iso3), "_survey_behav")
run_commit_push(reports)

#' (If the above isn't working, here are separate ones)
run_commit_push("bwa_survey_behav")
run_commit_push("cmr_survey_behav")
run_commit_push("ken_survey_behav")
run_commit_push("lso_survey_behav")
run_commit_push("moz_survey_behav")
run_commit_push("mwi_survey_behav")
run_commit_push("nam_survey_behav")
run_commit_push("swz_survey_behav")
run_commit_push("tza_survey_behav")
run_commit_push("uga_survey_behav")
run_commit_push("zaf_survey_behav")
run_commit_push("zmb_survey_behav")
run_commit_push("zwe_survey_behav")

#' Make a plot of all available surveys for manuscript
run_commit_push("plot_available-surveys")

#' Process all the data into one file
run_commit_push("process_all-data")

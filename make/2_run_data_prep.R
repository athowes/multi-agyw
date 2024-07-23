#' Prepare PHIA data in the 14 countries that have surveys
iso3 <- c("CMR", "LSO", "MWI", "NAM", "SWZ", "TZA", "UGA", "ZMB", "ZWE", "KEN",
          "CIV", "ETH", "RWA", "MOZ")
reports <- paste0(tolower(iso3), "_survey_phia")
run_commit_push(reports)

# Prepare new THIS
run_commit_push("tza_data_survey_tza2022phia")

#' Prepare BAIS data in Botswana
run_commit_push("bwa_survey_bais")

#' Prepare CAF MICS data
run_commit_push("caf_survey_mics")
run_commit_push("caf_survey_mics_men")

#' Prepare sexual behaviour datasets in all countries
#' Cache (with .zip files) and download (DHS API) errors here!
#' Might try using tryCatch while loop instead
# iso3 <- multi.utils::priority_iso3()
# reports <- paste0(tolower(iso3), "_survey_behav")
# run_commit_push(reports)

#' (If the above isn't working, here are separate ones)
run_commit_push("ago_survey_behav")
run_commit_push("bdi_survey_behav")
run_commit_push("bfa_survey_behav") # excludes 1999 DHS because sexual behavior data for that survey seems to be missing
run_commit_push("bwa_survey_behav") #' [x]
run_commit_push("civ_survey_behav") # Exclude 2005 AIS
run_commit_push("cmr_survey_behav") #' [x]
run_commit_push("cod_survey_behav")
run_commit_push("cog_survey_behav")
run_commit_push("eth_survey_behav")
run_commit_push("gab_survey_behav") # RUNS WITH 2000 DHS excluded
run_commit_push("gha_survey_behav")
run_commit_push("gin_survey_behav") # Exclude 1999 DHS
# run_commit_push("hti_survey_behav")
run_commit_push("ken_survey_behav") #' [x]
run_commit_push("lbr_survey_behav")
run_commit_push("lso_survey_behav") #' [x]
run_commit_push("mli_survey_behav") # NEED TO EXCLUDE 2012 SURVEY DUE TO areas not aligning to regions COME BACK TO ME!!!
run_commit_push("moz_survey_behav") #' [x]
run_commit_push("mwi_survey_behav") #' [x]
run_commit_push("nam_survey_behav") #' [x]
run_commit_push("ner_survey_behav")
run_commit_push("rwa_survey_behav") # RUNS WITH 2000 and 2005 DHS excluded
run_commit_push("sle_survey_behav")
run_commit_push("swz_survey_behav") #' [x]
run_commit_push("tcd_survey_behav") # Exclude 2004 DHS
run_commit_push("tgo_survey_behav")
run_commit_push("tza_survey_behav") #' [x]
run_commit_push("uga_survey_behav") #' [x]
run_commit_push("zaf_survey_behav") #' [x]
run_commit_push("zmb_survey_behav") #' [x]
run_commit_push("zwe_survey_behav") #' [x]
run_commit_push("caf_survey_behav")
run_commit_push("ben_survey_behav")
run_commit_push("gmb_survey_behav")
run_commit_push("sen_survey_behav")

# Same for men

#' (If the above isn't working, here are separate ones)
run_commit_push("ago_survey_behav_men")
run_commit_push("bdi_survey_behav_men")
run_commit_push("bfa_survey_behav_men")
run_commit_push("bwa_survey_behav_men") #' [x]
run_commit_push("civ_survey_behav_men")
run_commit_push("cmr_survey_behav_men") #' [x]
run_commit_push("cod_survey_behav_men")
run_commit_push("cog_survey_behav_men")
run_commit_push("eth_survey_behav_men")
run_commit_push("gab_survey_behav_men")
run_commit_push("gha_survey_behav_men")
run_commit_push("gin_survey_behav_men")
# run_commit_push("hti_survey_behav_men")
run_commit_push("ken_survey_behav_men") #' [x]
run_commit_push("lbr_survey_behav_men")
run_commit_push("lso_survey_behav_men") #' [x]
run_commit_push("mli_survey_behav_men")
run_commit_push("moz_survey_behav_men") #' [x]
run_commit_push("mwi_survey_behav_men") #' [x]
run_commit_push("nam_survey_behav_men") #' [x]
run_commit_push("ner_survey_behav_men")
run_commit_push("rwa_survey_behav_men")
run_commit_push("sle_survey_behav_men")
run_commit_push("swz_survey_behav_men") #' [x]
run_commit_push("tcd_survey_behav_men")
run_commit_push("tgo_survey_behav_men")
run_commit_push("tza_survey_behav_men") #' [x]
run_commit_push("uga_survey_behav_men") #' [x]
run_commit_push("zaf_survey_behav_men") #' [x]
run_commit_push("zmb_survey_behav_men") #' [x]
run_commit_push("zwe_survey_behav_men") #' [x]
run_commit_push("caf_survey_behav_men")
run_commit_push("ben_survey_behav_men")
run_commit_push("gmb_survey_behav_men")
run_commit_push("sen_survey_behav_men")

#' #' Make a plot of all available surveys for manuscript
#' run_commit_push("plot_available-surveys")
#'
#' #' Same plot for men
#' run_commit_push("plot_available-surveys_men")

#' Process all the data into one file
run_commit_push("process_all-data")

#' Same for men
run_commit_push("process_all-data_men")

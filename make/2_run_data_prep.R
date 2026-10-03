#' Prepare PHIA data in the 14 countries that have surveys
iso3 <- c("CMR", "LSO", "MWI", "NAM", "SWZ", "TZA", "UGA", "ZMB", "ZWE", "KEN",
          "CIV", "ETH", "RWA", "MOZ")
reports <- paste0(tolower(iso3), "_survey_phia")
lapply(reports,orderly_run)

# Prepare new PHIA surveys
orderly_run("tza_data_survey_tza2022phia")
orderly_run("uga_data_survey_uga2020phia")
orderly_run("zmb_data_survey_zmb2021phia")
orderly_run("zwe_data_survey_zwe2020phia")
orderly_run("swz_data_survey_swz2021phia")
orderly_run("mwi_data_survey_mwi2020phia")
orderly_run("lso_data_survey_lso2020phia")

#' Prepare BAIS data in Botswana
orderly_run("bwa_survey_bais")
orderly_run("bwa_survey_bais_v")

#' Prepare MICS data
orderly_run("caf_survey_mics")
orderly_run("caf_survey_mics_men")
orderly_run("mwi_survey_mics2019")
orderly_run("mwi_survey_mics2019_men")
orderly_run("gmb_survey_mics2018")
orderly_run("gmb_survey_mics2018_men")
orderly_run("gha_survey_mics2017")
orderly_run("gha_survey_mics2017_men")
orderly_run("sle_survey_mics2017")
orderly_run("sle_survey_mics2017_men")
orderly_run("swz_survey_mics2021")
orderly_run("swz_survey_mics2021_men")
orderly_run("nga_survey_mics2016")
orderly_run("nga_survey_mics2016_men")
orderly_run("nga_survey_mics2021")
orderly_run("nga_survey_mics2021_men")


#' Prepare sexual behaviour datasets in all countries
#' Cache (with .zip files) and download (DHS API) errors here!
#' Might try using tryCatch while loop instead
# iso3 <- multi.utils::priority_iso3()
# reports <- paste0(tolower(iso3), "_survey_behav")
# run_commit_push(reports)

#' (If the above isn't working, here are separate ones)
orderly_run("ago_survey_behav")
orderly_run("bdi_survey_behav")
orderly_run("bfa_survey_behav") # excludes 1999 DHS because sexual behavior data for that survey seems to be missing
orderly_run("bwa_survey_behav") #' [x]
orderly_run("civ_survey_behav") # Exclude 2005 AIS
orderly_run("cmr_survey_behav") #' [x]
orderly_run("cod_survey_behav")
orderly_run("cog_survey_behav")
orderly_run("eth_survey_behav")
orderly_run("gab_survey_behav") # RUNS WITH 2000 DHS excluded
orderly_run("gha_survey_behav")
orderly_run("gin_survey_behav") # Exclude 1999 DHS
# run_commit_push("hti_survey_behav")
orderly_run("ken_survey_behav") #' [x]
orderly_run("lbr_survey_behav")
orderly_run("lso_survey_behav") #' [x]
orderly_run("mli_survey_behav") # NEED TO EXCLUDE 2012 SURVEY DUE TO areas not aligning to regions COME BACK TO ME!!!
orderly_run("moz_survey_behav") #' [x]
orderly_run("mwi_survey_behav") #' [x]
orderly_run("nam_survey_behav") #' [x]
orderly_run("ner_survey_behav")
orderly_run("rwa_survey_behav") # RUNS WITH 2000 and 2005 DHS excluded
orderly_run("sle_survey_behav")
orderly_run("swz_survey_behav") #' [x]
orderly_run("tcd_survey_behav") # Exclude 2004 DHS
orderly_run("tgo_survey_behav")
orderly_run("tza_survey_behav") #' [x]
orderly_run("uga_survey_behav") #' [x]
# run_commit_push("zaf_survey_behav") #' [x]
orderly_run("zmb_survey_behav") #' [x]
orderly_run("zwe_survey_behav") #' [x]
orderly_run("caf_survey_behav")
orderly_run("ben_survey_behav")
orderly_run("gmb_survey_behav")
orderly_run("sen_survey_behav")
orderly_run("nga_survey_behav")

# Same for men

#' (If the above isn't working, here are separate ones)
orderly_run("ago_survey_behav_men")
orderly_run("bdi_survey_behav_men")
orderly_run("bfa_survey_behav_men")
orderly_run("bwa_survey_behav_men") #' [x]
orderly_run("civ_survey_behav_men")
orderly_run("cmr_survey_behav_men") #' [x]
orderly_run("cod_survey_behav_men")
orderly_run("cog_survey_behav_men")
orderly_run("eth_survey_behav_men")
orderly_run("gab_survey_behav_men")
orderly_run("gha_survey_behav_men")
orderly_run("gin_survey_behav_men")
# run_commit_push("hti_survey_behav_men")
orderly_run("ken_survey_behav_men") #' [x]
orderly_run("lbr_survey_behav_men")
orderly_run("lso_survey_behav_men") #' [x]
orderly_run("mli_survey_behav_men")
orderly_run("moz_survey_behav_men") #' [x]
orderly_run("mwi_survey_behav_men") #' [x]
orderly_run("nam_survey_behav_men") #' [x]
orderly_run("ner_survey_behav_men")
orderly_run("rwa_survey_behav_men")
orderly_run("sle_survey_behav_men")
orderly_run("swz_survey_behav_men") #' [x]
orderly_run("tcd_survey_behav_men")
orderly_run("tgo_survey_behav_men")
orderly_run("tza_survey_behav_men") #' [x]
orderly_run("uga_survey_behav_men") #' [x]
# run_commit_push("zaf_survey_behav_men") #' [x]
orderly_run("zmb_survey_behav_men") #' [x]
orderly_run("zwe_survey_behav_men") #' [x]
orderly_run("caf_survey_behav_men")
orderly_run("ben_survey_behav_men")
orderly_run("gmb_survey_behav_men")
orderly_run("sen_survey_behav_men")
orderly_run("nga_survey_behav_men")


#' #' Make a plot of all available surveys for manuscript
#' run_commit_push("plot_available-surveys")
#'
#' #' Same plot for men
#' run_commit_push("plot_available-surveys_men")

#' Process all the data into one file
orderly_run("process_all-data")

#' Same for men
orderly_run("process_all-data_men")

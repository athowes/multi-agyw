#' 1. Pull the Naomi area files as reports which can be depended upon
src("_make/pull_naomi_areas.R")

#' 2. Pull the Naomi population data as global data
src("_make/pull_naomi3.R")

#' 3. Run each of the _data_survey_behav scripts to prepare the input data for modelling
src("_make/run_data_prep.R")

#' 4. Run the four category single survey models for applicable countries
src("_make/run_fit_multi-sexbehav-sae.R")

#' 5. Run the three category multi-survey models for all countries
src("_make/run_fit_multi-sexbehav-sae.R")

#' 6. Post processing of the modelling results
src("_make/post_process.R")

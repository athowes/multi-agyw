#' 1. Pull the Naomi area files as reports which can be depended upon
src("make/pull_naomi_areas.R")

#' 2.  Pull the population data from Oli's fertility repo as reports which can be depended upon
src("make/pull_oli_population.R")

#' 3. Run each of the _data_survey_behav scripts to prepare the input data for modelling
src("make/run_data_prep.R")

#' 4. Run the four category models for applicable countries
src("make/run_aaa_fit_4-multinomial-smoothed-district-sexbehav.R")

#' 5. Run the three category models for all countries
src("make/run_aaa_fit_3-multinomial-smoothed-district-sexbehav.R")

#' TODO: Run the three category models for all countries jointly
# src("make/run_fit_3-multisexbehav-sae.R")

#' 6. Post processing of the modelling results
src("make/post_process.R")

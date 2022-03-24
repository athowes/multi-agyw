#' 1. Pull the Naomi area files as reports which can be depended upon
src("make/pull_naomi_areas.R")

#' 2.  Pull the population data from Oli's fertility repo as reports which can be depended upon
src("make/pull_oli_population.R")

#' 3. Run each of the _data_survey_behav scripts to prepare the input data for modelling
src("make/run_data_prep.R")

#' 4. Run the separate country models for applicable countries
src("make/run_aaa_fit_multi-sexbehav-sae.R")

#' 5. Run the three category models for all countries jointly
src("make/run_fit_multi-sexbehav-sae.R")

#' 6. Differentiate the highest risk cateogry using a logistic regression model
src("make/high_risk_differentiation.R")

#' 7. Post processing (including e.g. figures) of the modelling results
src("make/post_process.R")

#' 8. Create documentation
src("make/docs.R")

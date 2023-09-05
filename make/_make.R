# src_here <- function(x) src(here::here(x))

#' 1. Pull the Naomi area files
# src_here("make/pull_naomi_areas.R")

#' 2.  Pull Naomi data
# src_here("make/pull_naomi.R")

#' 3. Run each of the survey_behav scripts to prepare the data for modelling
# src_here("make/run_data_prep.R")

#' 4. Run the models for each country individually
# src_here("make/run_aaa_fit_multi-sexbehav-sae.R")

#' 5. Run the models for all countries jointly
# src_here("make/run_fit_multi-sexbehav-sae.R")

#' 6. Differentiate the highest risk category using a logistic regression model
# src_here("make/high_risk_differentiation.R")

#' 7. Post processing (including e.g. figures) of the modelling results
# src_here("make/post_process.R")

#' 8. Create documentation, presentations, posters, etc.
# src_here("make/docs.R")

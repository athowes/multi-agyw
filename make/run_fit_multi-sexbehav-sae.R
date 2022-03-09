source("make/utils.R")

#' Data pre-processing step
run_commit_push("process_all-data", push = FALSE)

#' Run model for all countries
id <- orderly::orderly_run("fit_multi-sexbehav-sae", parameters = list(lightweight = TRUE, fewer_countries = TRUE))
orderly::orderly_commit(id)

id <- orderly::orderly_run("fit_multi-sexbehav-sae", parameters = list(lightweight = TRUE))
orderly::orderly_commit(id)

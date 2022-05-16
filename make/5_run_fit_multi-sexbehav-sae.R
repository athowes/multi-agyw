source(here::here("make/utils.R"))

#' Run model for all countries
id <- orderly::orderly_run("fit_multi-sexbehav-sae", parameters = list(lightweight = TRUE, fewer_countries = TRUE))
orderly::orderly_commit(id)

#' Attempted to run this but R crash on 4x :'(
id <- orderly::orderly_run("fit_multi-sexbehav-sae", parameters = list(fewer_countries = TRUE))
orderly::orderly_commit(id)

id <- orderly::orderly_run("fit_multi-sexbehav-sae", parameters = list(lightweight = TRUE))
orderly::orderly_commit(id)

id <- orderly::orderly_run("fit_multi-sexbehav-sae")
orderly::orderly_commit(id)

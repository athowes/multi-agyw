#' Run model for all countries

#' Options for reducing computation in testing:
#' @param `lightweight` Fit just one model rather than all (eight) considered models
#' @param `fewer_countries` Use five (BWA, MOZ, MWI, ZMB, ZWE) out of the total countries

orderly::orderly_run("fit_multi-sexbehav-sae", parameters = list(top_year=2025))
# id <- orderly::orderly_run("fit_multi-sexbehav-sae", parameters = list(lightweight = TRUE))
# orderly::orderly_commit(id) #' [x]

orderly::orderly_run("fit_multi-sexbehav-sae_men", parameters = list(top_year=2025))
# id <- orderly::orderly_run("fit_multi-sexbehav-sae_men", parameters = list(lightweight = TRUE))
# orderly::orderly_commit(id) #' [x]

#' Run model for all countries

#' Options for reducing computation in testing:
#' @param `lightweight` Fit just one model rather than all (eight) considered models
#' @param `fewer_countries` Use five (BWA, MOZ, MWI, ZMB, ZWE) out of the thirteen total countries

id <- orderly::orderly_run("fit_multi-sexbehav-sae", parameters = list(lightweight = TRUE, fewer_countries = TRUE))
orderly::orderly_commit(id) #' [x]

id <- orderly::orderly_run("fit_multi-sexbehav-sae", parameters = list(fewer_countries = TRUE))
orderly::orderly_commit(id) #' [ ]

id <- orderly::orderly_run("fit_multi-sexbehav-sae", parameters = list(lightweight = TRUE))
orderly::orderly_commit(id) #' [x]

id <- orderly::orderly_run("fit_multi-sexbehav-sae", parameters = list(include_interactions = FALSE))
orderly::orderly_commit(id) #' [x]

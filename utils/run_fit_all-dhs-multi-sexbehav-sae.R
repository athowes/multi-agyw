#' Run fit_all-dhs-multi-sexbehav-sae in bulk

iso3_vec <- c("CMR", "KEN", "LSO", "MOZ", "MWI", "NAM", "SWZ", "TZA", "UGA", "ZAF", "ZMB", "ZWE")
iso3_vec <- c("CMR", "MOZ", "MWI", "ZMB", "ZWE")

params <- data.frame(iso3 = iso3_vec)
ids <- orderly::orderly_batch("aaa_fit_all-dhs-multi-sexbehav-sae", params)

#' Running individually
#' ##: the models have successfully run
#' #: the models have not yet successfully run

## id <- orderly::orderly_run("aaa_fit_all-dhs-multi-sexbehav-sae", parameters = list(iso3 = "CMR"))
#' Issue with not having population data for CMR? The aggregate is not working here for some reason

# id <- orderly::orderly_run("aaa_fit_all-dhs-multi-sexbehav-sae", parameters = list(iso3 = "KEN"))
#' Data creation issue #009
#' Begin fitting Model 7.
#' inla.mkl: src/inla.c:23905: inla_parse_ffield: Assertion `def->n > 1' failed.

# id <- orderly::orderly_run("aaa_fit_all-dhs-multi-sexbehav-sae", parameters = list(iso3 = "LSO"))
#' Data creation issue #009
#' Begin fitting Model 7.
#' inla.mkl: src/inla.c:23905: inla_parse_ffield: Assertion `def->n > 1' failed.

## id <- orderly::orderly_run("aaa_fit_all-dhs-multi-sexbehav-sae", parameters = list(iso3 = "MOZ"))

## id <- orderly::orderly_run("aaa_fit_all-dhs-multi-sexbehav-sae", parameters = list(iso3 = "MWI"))

# id <- orderly::orderly_run("aaa_fit_all-dhs-multi-sexbehav-sae", parameters = list(iso3 = "NAM"))
#' Begin fitting Model 5.
#' *** Warning *** Numerical error gives b22 = -8.79609e+12 <= 0.0 for idx=586. setting b22=1.49167e-154
#' Begin fitting Model 7.
#' inla.mkl: src/inla.c:23905: inla_parse_ffield: Assertion `def->n > 1' failed.

# id <- orderly::orderly_run("aaa_fit_all-dhs-multi-sexbehav-sae", parameters = list(iso3 = "SWZ"))
#' Begin fitting Model 7.
#' inla.mkl: src/inla.c:23905: inla_parse_ffield: Assertion `def->n > 1' failed.

# id <- orderly::orderly_run("aaa_fit_all-dhs-multi-sexbehav-sae", parameters = list(iso3 = "TZA"))
#' Data creation issue #009
#' Begin fitting Model 7.
#' inla.mkl: src/inla.c:23905: inla_parse_ffield: Assertion `def->n > 1' failed.

# id <- orderly::orderly_run("aaa_fit_all-dhs-multi-sexbehav-sae", parameters = list(iso3 = "UGA"))
#' Data creation issue #009
#' Begin fitting Model 7.
#' inla.mkl: src/inla.c:23905: inla_parse_ffield: Assertion `def->n > 1' failed.

# id <- orderly::orderly_run("aaa_fit_all-dhs-multi-sexbehav-sae", parameters = list(iso3 = "ZAF"))
#' Begin fitting Model 7.
#' inla.mkl: src/inla.c:23905: inla_parse_ffield: Assertion `def->n > 1' failed.

## id <- orderly::orderly_run("aaa_fit_all-dhs-multi-sexbehav-sae", parameters = list(iso3 = "ZMB"))

## id <- orderly::orderly_run("aaa_fit_all-dhs-multi-sexbehav-sae", parameters = list(iso3 = "ZWE"))

recent_ids <- sapply(
  iso3_vec,
  function(iso3) {
    orderly::orderly_search(query = paste0("latest(parameter:iso3 == '", iso3, "')"), "aaa_fit_all-dhs-multi-sexbehav-sae", draft = TRUE)
  }
)

#' Commit them (if not already in the archive?)
sapply(na.omit(recent_ids), function(id) orderly::orderly_commit(id))

#' Push to remote
sapply(na.omit(recent_ids), function(id) orderly::orderly_push_archive(name = "aaa_fit_all-dhs-multi-sexbehav-sae", id = id))

archived_recent_ids <- sapply(
  iso3_vec,
  function(iso3) {
    orderly::orderly_search(query = paste0("latest(parameter:iso3 == '", iso3, "')"), "aaa_fit_all-dhs-multi-sexbehav-sae")
  }
)

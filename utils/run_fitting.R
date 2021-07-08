#' Run everything in bulk
iso3_vec <- c("CMR", "KEN", "LSO", "MOZ", "MWI", "NAM", "SWZ", "TZA", "UGA", "ZAF", "ZMB", "ZWE")
params <- data.frame(iso3 = iso3_vec)
ids <- orderly::orderly_batch("aaa_fit_multi-sexbehav-sae", params)

#' Running individually
# id <- orderly::orderly_run("aaa_fit_multi-sexbehav-sae", parameters = list(iso3 = "CMR"))
# id <- orderly::orderly_run("aaa_fit_multi-sexbehav-sae", parameters = list(iso3 = "KEN"))
# id <- orderly::orderly_run("aaa_fit_multi-sexbehav-sae", parameters = list(iso3 = "LSO"))
# id <- orderly::orderly_run("aaa_fit_multi-sexbehav-sae", parameters = list(iso3 = "MOZ"))
# id <- orderly::orderly_run("aaa_fit_multi-sexbehav-sae", parameters = list(iso3 = "MWI"))
# id <- orderly::orderly_run("aaa_fit_multi-sexbehav-sae", parameters = list(iso3 = "NAM"))
# id <- orderly::orderly_run("aaa_fit_multi-sexbehav-sae", parameters = list(iso3 = "SWZ"))
# id <- orderly::orderly_run("aaa_fit_multi-sexbehav-sae", parameters = list(iso3 = "TZA"))
# id <- orderly::orderly_run("aaa_fit_multi-sexbehav-sae", parameters = list(iso3 = "UGA"))
# id <- orderly::orderly_run("aaa_fit_multi-sexbehav-sae", parameters = list(iso3 = "ZAF"))
# id <- orderly::orderly_run("aaa_fit_multi-sexbehav-sae", parameters = list(iso3 = "ZMB"))
# id <- orderly::orderly_run("aaa_fit_multi-sexbehav-sae", parameters = list(iso3 = "ZWE"))

#' Get the most recent archived or drafted version of each of the reports
#' Could also use ids from orderly_batch here
recent_ids <- sapply(
  iso3_vec,
  function(iso3) {
    orderly::orderly_search(query = paste0("latest(parameter:iso3 == '", iso3, "')"), "aaa_fit_multi-sexbehav-sae", draft = TRUE)
  }
)

#' Commit them (if not already in the archive?)
sapply(na.omit(recent_ids), function(id) orderly::orderly_commit(id))

#' Push to remote
sapply(na.omit(recent_ids), function(id) orderly::orderly_push_archive(name = "aaa_fit_multi-sexbehav-sae", id = id))

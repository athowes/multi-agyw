#' Run aaa_fit_3-multi-sexbehav-sae in bulk
#' For all 13 countries

report <- "aaa_fit_3-multi-sexbehav-sae"
iso3 <- c("BWA", "CMR", "KEN", "LSO", "MOZ", "MWI", "NAM", "SWZ", "TZA", "UGA", "ZAF", "ZMB", "ZWE")
params <- data.frame(iso3 = iso3, include_interactions = TRUE)
ids <- orderly::orderly_batch(report, params)

#' Get the most recent archived or drafted version of each of the reports
#' Could also use ids from orderly_batch here: if you've just run it then they should be the same
recent_ids <- sapply(iso3,
  function(iso3) {
    orderly::orderly_search(query = paste0("latest(parameter:iso3 == '", iso3, "')"), report, draft = TRUE)
  }
)

#' Commit them
sapply(na.omit(recent_ids), function(id) orderly::orderly_commit(id))

#' Push to remote
sapply(na.omit(recent_ids), function(id) orderly::orderly_push_archive(name = report, id = id))

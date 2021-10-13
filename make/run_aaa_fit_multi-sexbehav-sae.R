#' Run aaa_fit_multi-sexbehav-sae in bulk

report <- "aaa_fit_multi-sexbehav-sae"
iso3 <- c("CMR", "MWI", "ZAF", "ZMB", "ZWE") #' Only those countries which have survey question V7191A
params <- data.frame(iso3 = iso3)
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

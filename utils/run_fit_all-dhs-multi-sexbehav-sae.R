#' Run fit_all-dhs-multi-sexbehav-sae in bulk

iso3_vec <- c("BWA", "CMR", "KEN", "LSO", "MOZ", "MWI", "NAM", "SWZ", "TZA", "UGA", "ZAF", "ZMB", "ZWE")

params <- data.frame(iso3 = iso3_vec, include_interactions = TRUE)
ids <- orderly::orderly_batch("aaa_fit_all-dhs-multi-sexbehav-sae", params)

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

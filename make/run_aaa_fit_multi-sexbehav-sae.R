 #' Run aaa_fit_multi-sexbehav-sae in bulk

report <- "aaa_fit_multi-sexbehav-sae"

#' Three categories, for all 13 countries
priority_iso3 <- multi.utils::priority_iso3()

#' Lightweight, without PHIA, without interactions
params <- data.frame(iso3 = priority_iso3, lightweight = TRUE)
ids <- orderly::orderly_batch(report, params)

#' Get the most recent archived or drafted version of each of the reports
#' Could also use ids from orderly_batch here: if you've just run it then they should be the same
recent_ids <- sapply(priority_iso3, function(iso3) {
    orderly::orderly_search(
      query = paste0("latest(parameter:iso3 == '", iso3, "' && parameter:lightweight == TRUE)"),
      report,
      draft = TRUE
    )
})

#' Commit them and push to remote
sapply(na.omit(recent_ids), function(id) orderly::orderly_commit(id))
sapply(na.omit(recent_ids), function(id) orderly::orderly_push_archive(name = report, id = id))

#' Four categories, for only those countries which have any surveys with a dedicated sex paid question
recent <- orderly::orderly_latest(name = "plot_available-surveys")
available_surveys <- read_csv(paste0("archive/plot_available-surveys/", recent, "/available-surveys.csv"))

giftsvar_iso3 <- available_surveys %>%
  filter(giftsvar == 1) %>%
  pull(iso3) %>%
  unique()

params <- data.frame(iso3 = giftsvar_iso3, lightweight = TRUE, three_category = FALSE)
ids <- orderly::orderly_batch(report, params)

recent_ids <- sapply(giftsvar_iso3, function(iso3) {
  orderly::orderly_search(
    query = paste0("latest(parameter:iso3 == '", iso3, "' && parameter:lightweight == TRUE && parameter:three_category == FALSE)"),
    report,
    draft = TRUE
  )
})

sapply(na.omit(recent_ids), function(id) orderly::orderly_commit(id))
sapply(na.omit(recent_ids), function(id) orderly::orderly_push_archive(name = report, id = id))

run_commit_push("process_multi-sexbehav-sae")

#' Run aaa_fit_4-multi-sexbehav-sae in bulk

report <- "aaa_fit_4-multi-sexbehav-sae"

#' Only those countries which have any surveys with a dedicated sex paid question
recent <- orderly::orderly_latest(name = "plot_available-surveys")
available_surveys <- read_csv(paste0("archive/plot_available-surveys/", recent, "/available-surveys.csv"))

iso3 <- available_surveys %>%
  filter(giftsvar == 1) %>%
  pull(iso3) %>%
  unique()

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

 #' Run aaa_fit_multi-sexbehav-sae in bulk
report <- "aaa_fit_multi-sexbehav-sae"

#' Three categories, for all 13 countries
priority_iso3 <- multi.utils::priority_iso3()

#' Lightweight, including PHIA
params <- data.frame(iso3 = priority_iso3, lightweight = TRUE)
# ids <- orderly::orderly_batch(report, params)

orderly::orderly_run(report, filter(params, iso3 == "AGO")) #' [x]
orderly::orderly_run(report, filter(params, iso3 == "BDI")) #' [x]
orderly::orderly_run(report, filter(params, iso3 == "BFA")) #' [x]
orderly::orderly_run(report, filter(params, iso3 == "BWA")) #' [x]
orderly::orderly_run(report, filter(params, iso3 == "CIV")) #' [x]
orderly::orderly_run(report, filter(params, iso3 == "CMR")) #' [x]
orderly::orderly_run(report, filter(params, iso3 == "COD")) #' [x]
orderly::orderly_run(report, filter(params, iso3 == "ETH")) #' [x]
orderly::orderly_run(report, filter(params, iso3 == "GAB")) #' [x]
orderly::orderly_run(report, filter(params, iso3 == "GHA")) #' [x]
orderly::orderly_run(report, filter(params, iso3 == "GIN")) #' [x]
orderly::orderly_run(report, filter(params, iso3 == "HTI")) #' [x]
orderly::orderly_run(report, filter(params, iso3 == "KEN")) #' [x]
orderly::orderly_run(report, filter(params, iso3 == "LBR")) #' [x]
orderly::orderly_run(report, filter(params, iso3 == "LSO")) #' [x]
orderly::orderly_run(report, filter(params, iso3 == "MLI")) #' [x]
# orderly::orderly_run(report, filter(params, iso3 == "MOZ")) #' [x]
orderly::orderly_run(report, filter(params, iso3 == "MWI")) #' [x]
orderly::orderly_run(report, filter(params, iso3 == "NAM")) #' [x]
orderly::orderly_run(report, filter(params, iso3 == "NER")) #' [x]
orderly::orderly_run(report, filter(params, iso3 == "RWA")) #' [x]
orderly::orderly_run(report, filter(params, iso3 == "SLE")) #' [x]
orderly::orderly_run(report, filter(params, iso3 == "SWZ")) #' [x]
orderly::orderly_run(report, filter(params, iso3 == "TCD")) #' [x]
orderly::orderly_run(report, filter(params, iso3 == "TGO")) #' [x]
orderly::orderly_run(report, filter(params, iso3 == "TZA")) #' [x]
orderly::orderly_run(report, filter(params, iso3 == "UGA")) #' [x]
orderly::orderly_run(report, filter(params, iso3 == "ZAF")) #' [x]
orderly::orderly_run(report, filter(params, iso3 == "ZMB")) #' [x]
orderly::orderly_run(report, filter(params, iso3 == "ZWE")) #' [x]

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


########################################
# Men
########################################
#' Run aaa_fit_multi-sexbehav-sae in bulk
report <- "aaa_fit_multi-sexbehav-sae_men"

#' Three categories, for all 13 countries
priority_iso3 <- multi.utils::priority_iso3()

#' Lightweight, including PHIA
params <- data.frame(iso3 = priority_iso3, lightweight = TRUE)
# ids <- orderly::orderly_batch(report, params)

orderly::orderly_run(report, filter(params, iso3 == "AGO")) #' [x]
orderly::orderly_run(report, filter(params, iso3 == "BDI")) #' [x]
orderly::orderly_run(report, filter(params, iso3 == "BFA")) #' [x]
orderly::orderly_run(report, filter(params, iso3 == "BWA")) #' [x]
orderly::orderly_run(report, filter(params, iso3 == "CIV")) #' [x]
orderly::orderly_run(report, filter(params, iso3 == "CMR")) #' [x]
orderly::orderly_run(report, filter(params, iso3 == "COD")) #' [x]
orderly::orderly_run(report, filter(params, iso3 == "ETH")) #' [x]
orderly::orderly_run(report, filter(params, iso3 == "GAB")) #' [x]
orderly::orderly_run(report, filter(params, iso3 == "GHA")) #' [x]
orderly::orderly_run(report, filter(params, iso3 == "GIN")) #' [x]
orderly::orderly_run(report, filter(params, iso3 == "HTI")) #' [x]
orderly::orderly_run(report, filter(params, iso3 == "KEN")) #' [x]
orderly::orderly_run(report, filter(params, iso3 == "LBR")) #' [x]
orderly::orderly_run(report, filter(params, iso3 == "LSO")) #' [x]
orderly::orderly_run(report, filter(params, iso3 == "MLI")) #' [x]
# orderly::orderly_run(report, filter(params, iso3 == "MOZ")) #' [x]
orderly::orderly_run(report, filter(params, iso3 == "MWI")) #' [x]
orderly::orderly_run(report, filter(params, iso3 == "NAM")) #' [x]
orderly::orderly_run(report, filter(params, iso3 == "NER")) #' [x]
orderly::orderly_run(report, filter(params, iso3 == "RWA")) #' [x]
orderly::orderly_run(report, filter(params, iso3 == "SLE")) #' [x]
orderly::orderly_run(report, filter(params, iso3 == "SWZ")) #' [x]
orderly::orderly_run(report, filter(params, iso3 == "TCD")) #' [x]
orderly::orderly_run(report, filter(params, iso3 == "TGO")) #' [x]
orderly::orderly_run(report, filter(params, iso3 == "TZA")) #' [x]
orderly::orderly_run(report, filter(params, iso3 == "UGA")) #' [x]
orderly::orderly_run(report, filter(params, iso3 == "ZAF")) #' [x]
orderly::orderly_run(report, filter(params, iso3 == "ZMB")) #' [x]
orderly::orderly_run(report, filter(params, iso3 == "ZWE")) #' [x]

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




run_commit_push("process_multi-sexbehav-sae")

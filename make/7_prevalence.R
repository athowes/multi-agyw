
#' Calculate and plot HIV prevalence by risk group
#' @param `survey_year_sample` what year we should sample from the sexual behaviour survey data
#' most recent survey year appropriate if running individual country

id <- orderly::orderly_run("process_prevalence", parameters = list(survey_year_sample = 2018))
orderly::orderly_commit(id) #' [x]

id <- orderly::orderly_run("process_prevalence_men", parameters = list(survey_year_sample = 2018))
orderly::orderly_commit(id) #' [x]

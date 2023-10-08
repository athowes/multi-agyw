
#' Calculate and plot HIV incidence by risk group
run_commit_push("process_incidence") #' [x]
run_commit_push("process_incidence_men") #' [x]

#' @param `survey_year_sample` what year we should sample from the sexual behaviour survey data
#' most recent survey year appropriate if running individual country

id <- orderly::orderly_run("process_incidence", parameters = list(survey_year_sample = 2018))
orderly::orderly_commit(id) #' [x]

id <- orderly::orderly_run("process_incidence_men", parameters = list(survey_year_sample = 2018))
orderly::orderly_commit(id) #' [x]

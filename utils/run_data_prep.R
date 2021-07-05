#' The 13 GF AGYW countries minus BWA
iso3 <- c("CMR", "KEN", "LSO", "MOZ", "MWI", "NAM", "SWZ", "TZA", "UGA", "ZAF", "ZMB", "ZWE")

#' The names of the reports to run
reports <- paste0(tolower(iso3), "_data_survey_behav")

#' Try to run them
sapply(
  reports,
  function(report) {
    #' Run the report
    tryCatch(
      id <- orderly::orderly_run(report),
      error = function(e) paste0("Report ", report, " failed to run.")
    )
    #' Commit it to archive
    tryCatch(
      orderly::orderly_commit(id),
      error = function(e) paste0("Report ", report, " failed to commit.")
    )
    #' Push archive to remote
    tryCatch(
      orderly::orderly_push_archive(report),
      error = function(e) paste0("Report ", report, " could not be pushed to the remote.")
    )
  }
)

#' Running an individual report
# report <- "name_here"
# id <- orderly::orderly_run(report)
# orderly::orderly_commit(id)
# orderly::orderly_push_archive(report)

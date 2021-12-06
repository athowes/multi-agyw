#' Run and optionally commit or push a collection of reports.
#'
#' @param `reports` A vector of report names.
#' @param `commit` Should the reports be commited? Defaults to `TRUE`.
#' @param `push` Should the reports be commited? Defaults to `TRUE`.
run_commit_push <- function(reports, commit = TRUE, push = TRUE) {
  sapply(
    reports,
    function(report) {
      id <- orderly::orderly_run(report)
      if(commit) { orderly::orderly_commit(id) }
      if(push) { orderly::orderly_push_archive(report) }
    }
  )
}

#' Prepare PHIA data in the 8 GF AGYW countries that have surveys
iso3 <- c("CMR", "MWI", "NAM", "SWZ", "TZA", "UGA", "ZMB", "ZWE")
reports <- paste0(tolower(iso3), "_survey_phia")
run_commit_push(reports, push = FALSE)

#' Prepare BAIS data in Botswana
run_commit_push("bwa_survey_bais", push = FALSE)

#' Prepare sexual behaviour datasets in the 13 GF AGYW countries minus BWA
iso3 <- c("BWA", "CMR", "KEN", "LSO", "MOZ", "MWI", "NAM", "SWZ", "TZA", "UGA", "ZAF", "ZMB", "ZWE")
reports <- paste0(tolower(iso3), "_survey_behav")
run_commit_push(reports, push = FALSE)

#' Make a plot of all available surveys for manuscript
run_commit_push("plot_available-surveys", push = FALSE)

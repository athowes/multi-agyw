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

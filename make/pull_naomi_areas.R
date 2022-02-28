priority_iso3 <- multi.utils::priority_iso3()

#' Names of the reports to pull
reports <- paste0(tolower(iso3), "_data_areas")

sapply(reports,
  function(report) {
    tryCatch(
      orderly::orderly_pull_archive(report, remote = "naomi2"),
      error = function(e) paste0("Report ", report, " could not be pulled!")
    )
  }
)

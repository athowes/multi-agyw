priority_iso3 <- multi.utils::priority_iso3()
missing_iso3 <- c("AGO", "CAF", "COD", "COG", "GAB", "GNQ", "RWA", "BDI")
iso3 <- c(priority_iso3, missing_iso3)

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

run_commit_push("national_data_areas")

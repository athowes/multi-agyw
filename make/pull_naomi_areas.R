#' The 13 GF AGYW countries
iso3 <- c("BWA", "CMR", "KEN", "LSO", "MOZ", "MWI", "NAM", "SWZ", "TZA", "UGA", "ZAF", "ZMB", "ZWE")

#' Check that there are indeed 13
length(iso3)

#' The names of the reports to pull
reports <- paste0(tolower(iso3), "_data_areas")

sapply(
  reports,
  function(report) {
    tryCatch(
      orderly::orderly_pull_archive(report, remote = "naomi2"),
      error = function(e) paste0("Report ", report, " could not be pulled!")
    )
  }
)

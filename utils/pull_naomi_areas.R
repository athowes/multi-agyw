#' The 13 GF AGYW countries minus BWA and minus MWI
iso3 <- c("CMR", "KEN", "LSO", "MOZ", "NAM", "SWZ", "TZA", "UGA", "ZAF", "ZMB", "ZWE")

#' Check that it is indeed 13 - 1 - 1 = 11
length(iso3)

#' The names of the reports to pull
reports <- paste0(tolower(iso3), "_data_areas")

sapply(
  reports,
  function(report) {
    tryCatch(
      orderly::orderly_pull_archive(report, remote = "naomi"),
      error = function(e) paste0("Report ", report, " could not be pulled!")
    )
  }
)

#' Malawi has a different remote
orderly::orderly_pull_archive("mwi_data_areas", remote = "malawi")

iso3 <- multi.utils::priority_iso3()

#' Names of the reports to pull
reports <- paste0(tolower(iso3), "_data_areas")

#' Using area files to correspond to the 2021 release of Naomi
#' (MWI and ZAF have their own repos, but their files are up to date for now)
sapply(reports,
  function(report) {
    tryCatch(
      orderly::orderly_pull_archive(
        report,
        remote = "naomi2",
        id = "latest(parameter:version == 2021)"
      ),
      error = function(e) paste0("Report ", report, " could not be pulled!")
    )
  }
)

#' For MOZ, ETH & TZA there are changes for the 2022 version
orderly::orderly_pull_archive("tza_data_areas", remote = "naomi2", id = "latest(parameter:version == 2022)")
orderly::orderly_pull_archive("moz_data_areas", remote = "naomi2", id = "latest(parameter:version == 2022)")
orderly::orderly_pull_archive("eth_data_areas", remote = "naomi2", id = "latest(parameter:version == 2022)")

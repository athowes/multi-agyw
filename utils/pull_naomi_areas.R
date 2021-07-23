#' The 13 GF AGYW countries
#' Excluding
#' * MWI (has separate remote)
#' * NAM, BWA (is in naomi2 remote, which Oli says I should be using for everything)
#' TODO: Check with Rachel about naomi and naomi2
iso3 <- c("CMR", "BWA", "KEN", "LSO", "MOZ", "SWZ", "TZA", "UGA", "ZAF", "ZMB", "ZWE")

#' Check that it is indeed 13 - 3 = 10
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

#' Malawi has a different remote (Shared Documents/orderly/mwi-hiv-orderly)
orderly::orderly_pull_archive("mwi_data_areas", remote = "malawi")

#' Namibia and Botswana has a different remote (Shared Documents/orderly/naomi-orderly-naomi2)
orderly::orderly_pull_archive("nam_data_areas", remote = "naomi2")
orderly::orderly_pull_archive("bwa_data_areas", remote = "naomi2")

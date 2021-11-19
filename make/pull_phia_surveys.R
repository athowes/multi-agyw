#' The countries which have processed PHIA data (from the 13 GF AGYW countries) so far
#' According to Documents/orderly/naomi-data-orderly/archive
iso3 <- c("LSO", "MWI", "NAM", "ZMB", "ZWE")

#' Names of the reports to pull
reports <- paste0(tolower(iso3), "_survey_phia")

sapply(reports, function(report) orderly::orderly_pull_archive(report, remote = "naomi"))

#' The 13 GF AGYW countries
priority_iso3 <- multi.utils::priority_iso3()
analysis_level <- multi.utils::analysis_level()

#' Population option 1: aaa_scale_pop reports from Oli's fertility repo
#' "Worldpop pixel level populations, overlaid with district level shape files, which are then
#' scaled to the 5 year age group and sex distributions from WPP 2019 at national level."
lapply(priority_iso3, function(x) {
  orderly::orderly_pull_archive(
    "aaa_scale_pop",
    id = paste0('latest(parameter:iso3 == "', x, '")'),
    remote = "fertility",
    recursive = TRUE)
})

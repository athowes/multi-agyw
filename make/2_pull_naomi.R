#' The 13 GF AGYW countries
priority_iso3 <- multi.utils::priority_iso3()
analysis_level <- multi.utils::analysis_level()

sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")

url <- "sites/HIVInferenceGroup-WP/Shared Documents/Data/Spectrum files/2022 naomi preliminary/r-extracts/indicators_with-moz2021.rds"
path <- sharepoint$download(URLencode(url))
naomi_output <- readRDS(path)

naomi_extract <- naomi_output %>%
  filter(
    iso3 %in% priority_iso3,
    indicator %in% c("population", "plhiv", "infections"),
    #' These are the age groups we are considering, plus those which are useful for disaggregation
    age_group_label %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "15-24", "15-49"),
    #' Only female
    sex == "female",
    #' Value being used in the global report (there is no CY2021Q4 in ZAF)
    ifelse(iso3 != "ZAF", calendar_quarter == "CY2021Q4", calendar_quarter == "CY2021Q3")
  ) %>%
  left_join(
    as.data.frame(analysis_level) %>%
      tibble::rownames_to_column("iso3"),
    by = "iso3"
  ) %>%
  filter(area_level %in% c(0, analysis_level)) %>%
  select(-analysis_level)

saveRDS(naomi_extract, "src/process_all-data/naomi_extract.rds")

#' Alternative population option: aaa_scale_pop reports from Oli's fertility repo
#' "Worldpop pixel level populations, overlaid with district level shape files, which are then
#' scaled to the 5 year age group and sex distributions from WPP 2019 at national level."
lapply(priority_iso3, function(x) {
  orderly::orderly_pull_archive(
    "aaa_scale_pop",
    id = paste0('latest(parameter:iso3 == "', x, '")'),
    remote = "fertility",
    recursive = TRUE)
})

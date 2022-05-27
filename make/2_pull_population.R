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

#' Population option 2: Naomi population data from Sharepoint
#' Get age-stratified population total sizes from Naomi model outputs
sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")

url <- "sites/HIVInferenceGroup-WP/Shared Documents/Data/Spectrum files/2021 naomi/Naomi datasets in R/naomi3.rds"
path <- sharepoint$download(URLencode(url))
naomi3 <- readRDS(path)

url <- "sites/HIVInferenceGroup-WP/Shared Documents/Data/Spectrum files/2021 naomi/areas-extract/naomi-2021-results_pooled-area-hierarchy.csv"
path <- sharepoint$download(URLencode(url))
area_hierarchy <- read_csv(path)

naomi3 <- naomi3 %>%
  filter(
    iso3 %in% priority_iso3,
    indicator_label %in% c("Population", "PLHIV", "New infections"),
    #' These are the age groups we are considering,
    age_group_label %in% c("15-19", "20-24", "25-29", "15-24", "15-49"),
    #' Only female
    sex == "female"
  ) %>%
  #' The most recent estimates
  group_by(iso3) %>%
  filter(calendar_quarter == max(calendar_quarter)) %>%
  ungroup() %>%
  #' Merge with area hierarchy data
  left_join(
    select(area_hierarchy, area_id, parent_area_id),
    by = "area_id"
  ) %>%
  select(
    iso3, area_id, area_level, age_group = age_group_label,
    indicator = indicator_label, estimate = mean, parent_area_id
  )

#' BWA and CMR are at one level too low
naomi3 %>%
  group_by(iso3) %>%
  summarise(area_level = unique(area_level)) %>%
  filter(area_level != 0) %>%
  t()

#' So lets aggregate them upwards here
naomi3_aggregates <- naomi3 %>%
  filter(iso3 %in% c("BWA", "CMR")) %>%
  group_by(parent_area_id, age_group, indicator) %>%
  summarise(
    iso3 = iso3,
    estimate = sum(estimate)
  ) %>%
  rename(area_id = parent_area_id) %>%
  mutate(area_level = as.numeric(substr(area_id, 5, 5))) %>%
  #' For some reason some of the rows are duplicated here
  #' Could investigate this more...
  distinct()

naomi3 <- bind_rows(naomi3, naomi3_aggregates) %>%
  split(.$iso3) %>%
  lapply(function(x) {
    #' Checking here that the area_level is correct
    filter(x, area_level == analysis_level[x$iso3[1]])
  }) %>%
  bind_rows()

naomi3_national <- naomi3 %>%
  group_by(iso3, age_group, indicator) %>%
  summarise(
    estimate = sum(estimate)
  ) %>%
  mutate(
    area_level = 0,
    area_id = iso3
  )

naomi3 <- bind_rows(naomi3, naomi3_national) %>%
  select(-parent_area_id)

saveRDS(naomi3, "global/naomi3-population-plhiv-infections.rds")

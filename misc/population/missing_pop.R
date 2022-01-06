library(tidyverse)

analysis_level <- c("BWA" = 2,
                    "CMR" = 2,
                    "KEN" = 2,
                    "LSO" = 1,
                    "MOZ" = 2,
                    "MWI" = 5,
                    "NAM" = 2,
                    "SWZ" = 1,
                    "TZA" = 3,
                    "UGA" = 3,
                    "ZAF" = 2,
                    "ZMB" = 2,
                    "ZWE" = 2)

naomi3 <- readRDS("global/naomi3.rds") %>%
  filter(
    indicator_label == "Population",
    calendar_quarter == max(calendar_quarter)
  )

#' Countries where there is missing Naomi population data are BWA, CMR, MOZ and ZAF

#' Only have BWA data at level 3 and want level 2
naomi3 %>%
  filter(iso3 == "BWA") %>%
  select(area_level) %>%
  unique()

analysis_level[["BWA"]]

#' Same story in CMR (want 2 and have 3)
naomi3 %>%
  filter(iso3 == "CMR") %>%
  select(area_level) %>%
  unique()

analysis_level[["CMR"]]

#' Have 2 and want 2 in MOZ
naomi3 %>%
  filter(iso3 == "MOZ") %>%
  select(area_level) %>%
  unique()

analysis_level[["MOZ"]]

moz_areas <- read_sf("archive/moz_data_areas/20201112-144347-48e53ce9/moz_areas.geojson")

#' 161 areas
moz_areas %>%
  filter(area_level == analysis_level[["MOZ"]]) %>%
  select(area_name) %>%
  nrow()

#' But only 144 in the Naomi population data!
naomi3 %>%
  filter(iso3 == "MOZ") %>%
  select(area_name) %>%
  unique() %>%
  nrow()

#' Don't have any Naomi data in ZAF
naomi3 %>%
  filter(iso3 == "ZAF") %>%
  select(area_level) %>%
  unique()

tmp <- every_all_dhs_multinomial_smoothed_district_sexbehav %>%
  filter(iso3 == "MOZ")


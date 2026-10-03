
#' ## Load area hierarchy
orderly_dependency("process_areas",
                   "latest()",
                   "zwe_areas.geojson")
areas <- read_sf("zwe_areas.geojson")

#' #' Authenticate SharePoint login
#' sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")
#'
#' #' Read in files from SharePoint
#' raw_path <- "sites/HIVInferenceGroup-WP/Shared%20Documents/Data/naomi-raw/ZWE/2021-01-28/ZIMPHIA2020_UNAIDS_DATASET_20201217.csv"
#' raw_path <- URLencode(raw_path)
#'
#' phia_path <- sharepoint$download(raw_path)

phia_path <- "ZWE2020PHIA/datasets"

phia_files <- list(geo = "ZIMPHIA 2020 Geospatial Data (DTA).zip",
                   survey = "ZIMPHIA 2020 Household (DTA).zip") %>%
  lapply(function(x) file.path(phia_path, x))

geo <- rdhs::read_zipdata(phia_files$geo)

hh <- rdhs::read_zipdata(phia_files$survey, "zimphia2020hh.dta")
bio <- rdhs::read_zipdata(phia_files$survey, "zimphia2020adultbio.dta")
ind <- rdhs::read_zipdata(phia_files$survey, "zimphia2020adultind.dta")


#' ## Load PHIA datasets
iso3 <- "ZWE"
survey_id  <- "ZWE2020PHIA"
survey_mid_calendar_quarter <- "CY2020Q1"

# phia_col_types <- "cciiiiiiiiiiiiddcdd"
#
# phia <- read_csv(phia_path, col_types = phia_col_types) %>%
#   mutate(survey_id = survey_id,
#          cluster_id = paste(varstrat, varunit) %>% as_factor %>% as.integer,
#          household = householdid %>% as_factor %>% as.integer,
#          line = personid)

phia <- ind %>%
  filter(indstatus == 1) %>%  # Respondent
  select(centroidid, province, urban, householdid,
         personid, surveystyear, surveystmonth,
         intwt0, gender, age) %>%
  full_join(
    bio %>%
      filter(bt_status == 1) %>%
      select(personid, btwt0, hivstatusfinal, arvstatus, artselfreported,
             vls, cd4count, recentlagvlarv),
    by = "personid"
  ) %>%
  mutate(survey_id = survey_id,
         cluster_id = centroidid)

#' ## Survey regions
#'
#' This table identifies the smallest area in the area hierarchy which contains each
#' region in the survey stratification, which is the smallest area to which a cluster
#' can be assigned with certainty. (Usually admin 1)

phia$survey_region_id <- phia$province # different in each survey dataset depending on survey stratification

survey_region_id <- c("Bulawayo" = 0,
                      "Manicaland" = 1,
                      "Mashonaland Central" = 2,
                      "Mashonaland East" = 3,
                      "Mashonaland West" = 4,
                      "Matabeleland North" = 5,
                      "Matabeleland South" = 6,
                      "Midlands" = 7,
                      "Masvingo" = 8,
                      "Harare" = 9)

areas %>% filter(area_level == 1) %>% select(area_id, area_name)

survey_region_area_id <- c("Bulawayo" = "ZWE_1_10",
                           "Manicaland" = "ZWE_1_12",
                           "Mashonaland Central" = "ZWE_1_13",
                           "Mashonaland East" = "ZWE_1_14",
                           "Mashonaland West" = "ZWE_1_15",
                           "Matabeleland North" = "ZWE_1_17",
                           "Matabeleland South" = "ZWE_1_18",
                           "Midlands" = "ZWE_1_19",
                           "Masvingo" = "ZWE_1_16",
                           "Harare" = "ZWE_1_11")

survey_regions <- tibble(survey_id = survey_id,
                         survey_region_id = survey_region_id,
                         survey_region_name = names(survey_region_id),
                         survey_region_area_id = survey_region_area_id[names(survey_region_id)])


#' Inspect area_id assigments to confirm

survey_regions %>%
  left_join(
    areas %>%
    as.data.frame() %>%
    select(area_id, area_name, area_level, area_level_label),
    by = c("survey_region_area_id" = "area_id")
  )


#' *** Should not require edits beyond this point ***

#' ## Survey clusters dataset
#'
#' This data frame maps survey clusters to the highest level in the area hiearchy
#' based on geomasked cluster centroids, additionally checking that the geolocated
#' areas are contained in the survey region.

survey_clusters <- phia %>%
  transmute(survey_id,
            cluster_id,
            res_type = factor(urban, 1:2, c("urban", "rural")),
            survey_region_id) %>%
  distinct() %>%
  left_join(geo, by = c("cluster_id" = "centroidid")) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), remove = FALSE) %>%
  sf::`st_crs<-`(4326)


#' Snap clusters to areas
#'
#' This is slow because it maps to the lowest level immediately
#' It would be more efficient to do this recursively through
#' the location hierarchy tree -- but not worth the effort right now.

#' Create a list of all of the areas within each survey region
#' (These are the candidate areas where a cluster could be located)

survey_region_areas  <- survey_regions %>%
  left_join(
    get_area_collection(as.data.frame(areas),
                        area_scope = survey_regions$survey_region_area_id),
    by = c("survey_region_area_id" = "area_scope")
  ) %>%
  left_join(
    areas %>% select(area_id, geometry),
    by = "area_id"
  ) %>%
  select(survey_region_id, area_id, geometry_area = geometry)

#' Seke district cuts into Harare due to Epworth --> Seke allocation.
#' Add this manually.

survey_region_areas <- survey_region_areas %>%
  rbind(
    areas %>%
    filter(area_name == "Seke") %>%
    mutate(survey_region_id = 9) %>%
    as.data.frame() %>%
    select(survey_region_id, area_id, geometry_area = geometry)
  )



#' Calculate distance to each candidate area for each cluster

survey_clusters <- survey_clusters %>%
  left_join(survey_region_areas, by = "survey_region_id") %>%
  mutate(
    distance = unlist(Map(sf::st_distance, geometry, geometry_area))
  )

#' Keep the area with the smallest distance from cluster centroid.
#' (Should be 0 for almost all)

survey_clusters <- survey_clusters %>%
  arrange(distance) %>%
  group_by(survey_id, cluster_id) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame %>%
  transmute(survey_id,
            cluster_id,
            res_type,
            survey_region_id,
            longitude,
            latitude,
            geoloc_area_id = area_id,
            geoloc_distance = distance)

#' Review clusters outside admin area

survey_clusters %>%
  filter(geoloc_distance > 0) %>%
  arrange(-geoloc_distance) %>%
  left_join(survey_regions)

#' ## Survey individuals dataset

#' Create individuals data
survey_individuals <- phia %>%
  group_by(survey_id) %>%  # for normalizing weights
  transmute(
    cluster_id,
    individual_id = personid,
    household = householdid,
    line = personid,
    interview_cmc = 12 * (surveystyear - 1900) + surveystmonth,
    sex = factor(gender, 1:2, c("male", "female")),
    age,
    dob_cmc = NA,
    indweight = intwt0
  ) %>%
  mutate(age = as.integer(age),
         indweight = indweight / mean(indweight, na.rm=TRUE))

survey_biomarker <- phia %>%
  group_by(survey_id) %>%  # for normalizing weights
  transmute(
    individual_id = personid,
    hivweight = btwt0,
    hivstatus = case_when(hivstatusfinal == 1 ~ 1,
                          hivstatusfinal == 2 ~ 0),
    arv = case_when(arvstatus == 1 ~ 1,
                    arvstatus == 2 ~ 0),
    artself = case_when(artselfreported == 1 ~ 1,
                        artselfreported == 2 ~ 0),
    vls = case_when(vls == 1 ~ 1,
                    vls == 2 ~ 0),
    cd4 = NA,
    recent = case_when(recentlagvlarv == 1 ~ 1,
                       recentlagvlarv == 2 ~ 0)
  ) %>%
  ungroup() %>%
  mutate(hivweight = hivweight / mean(hivweight, na.rm = TRUE))


#' ## Survey meta data

survey_meta <- survey_individuals %>%
  group_by(survey_id) %>%
  summarise(
    female_age_min = min(if_else(sex == "female", age, NA_integer_), na.rm=TRUE),
    ## female_age_max = max(if_else(sex == "female", age, NA_integer_), na.rm=TRUE),
    female_age_max = Inf,
    male_age_min = min(if_else(sex == "male", age, NA_integer_), na.rm=TRUE),
    ## male_age_max = max(if_else(sex == "male", age, NA_integer_), na.rm=TRUE),
    male_age_max = Inf
  ) %>%
  mutate(survey_mid_calendar_quarter = survey_mid_calendar_quarter) %>%
  ungroup()

survey_sexbehav <- extract_sexbehav_phia(ind, survey_id)
(misallocation <- check_survey_sexbehav(survey_sexbehav))

#' ## Save survey datasets

write_csv(survey_meta, paste0(tolower(survey_id), "_survey_meta.csv"), na = "")
write_csv(survey_regions, paste0(tolower(survey_id), "_survey_regions.csv"), na = "")
write_csv(survey_clusters, paste0(tolower(survey_id), "_survey_clusters.csv"), na = "")
write_csv(survey_individuals, paste0(tolower(survey_id), "_survey_individuals.csv"), na = "")
write_csv(survey_biomarker, paste0(tolower(survey_id), "_survey_biomarker.csv"), na = "")
write_csv(survey_sexbehav, paste0(tolower(survey_id), "_survey_sexbehav.csv"), na = "")

#' ## Calculate indicators by area/sex/age

# phia_survey_indicators <- calc_survey_hiv_indicators(
#                               survey_meta,
#                               survey_regions,
#                               survey_clusters,
#                               survey_individuals,
#                               survey_biomarker,
#                               as.data.frame(areas)
#                             )

#' ## Save survey indciators dataset
# write_csv(phia_survey_indicators, paste0(tolower(survey_id), "_hiv_indicators.csv"), na = "")



#' #' Authenticate SharePoint login
#' sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")
#'
#' #' Read in files from SharePoint
#' raw_path <- "sites/HIVInferenceGroup-WP/Shared Documents/Data/naomi-raw/LSO/2022-02-01 LePHIA 2020/LePHIA2020_UNAIDS_DATASET.csv"
#' raw_path <- URLencode(raw_path)
#' phia_path <- sharepoint$download(raw_path)

#' ## Load area hierarchy
orderly_dependency("process_areas",
                   "latest()",
                   "lso_areas.geojson")
areas <- read_sf("lso_areas.geojson")


#' ## Load PHIA datasets
iso3 <- "LSO"
survey_id  <- "LSO2020PHIA"
survey_year <- 2020
survey_mid_calendar_quarter <- "CY2020Q1"

# phia_col_types <- "cciiiiiiiiiiiiiiddcdd"
#
# phia <- read_csv(phia_path, col_types = phia_col_types) %>%
#   mutate(survey_id = survey_id,
#          cluster_id = paste(varstrat, varunit),
#          individual_id = personid,
#          household = householdid,
#          line = personid)

phia_path <- "LSO2020PHIA/datasets"

phia_files <- list(geo = "LePHIA 2020 Geospatial Data (DTA).zip",
                   survey = "LePHIA 2020 Household (DTA).zip") %>%
  lapply(function(x) file.path(phia_path, x))

geo <- rdhs::read_zipdata(phia_files$geo)
names(geo) <- tolower(names(geo))

hh <- rdhs::read_zipdata(phia_files$survey, "lephia2020hh.dta")
bio <- rdhs::read_zipdata(phia_files$survey, "lephia2020adultbio.dta")
ind <- rdhs::read_zipdata(phia_files$survey, "lephia2020adultind.dta")

phia <- ind %>%
  filter(indstatus == 1) %>%  # Respondent
  select(centroidid, district, urban, householdid,
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

phia$survey_region_id <- phia$district # different in each survey dataset depending on survey stratification

survey_region_id <- c("Maseru" = 1,
                      "Mafeteng" = 2,
                      "Mohale’s Hoek" = 3,
                      "Leribe" = 4,
                      "Berea" = 5,
                      "Quthing" = 6,
                      "Butha-Buthe" = 7,
                      "Mokhotlong" = 8,
                      "Qacha’s Nek" = 9,
                      "Thaba-Tseka" = 10)

areas %>% filter(area_level == 1) %>% select(area_id, area_name)


survey_region_area_id <- c("Maseru" = "LSO_1_1",
                           "Mafeteng" = "LSO_1_5",
                           "Mohale’s Hoek" = "LSO_1_6",
                           "Leribe" = "LSO_1_3",
                           "Berea" = "LSO_1_4",
                           "Quthing" = "LSO_1_7",
                           "Butha-Buthe" = "LSO_1_2",
                           "Mokhotlong" = "LSO_1_9",
                           "Qacha’s Nek" = "LSO_1_8",
                           "Thaba-Tseka" = "LSO_1_10")

survey_regions <- tibble(survey_id = survey_id,
                         survey_region_id = survey_region_id,
                         survey_region_name = names(survey_region_id),
                         survey_region_area_id = survey_region_area_id[names(survey_region_id)])


#' Add survey region boundary

survey_regions <- survey_regions %>%
  left_join(
    areas %>% select(survey_region_area_id = area_id)
  ) %>%
  st_as_sf()


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
  st_join(
    st_point_on_surface(areas) %>%
    filter(area_level == max(area_level)) %>%
    select(area_id)
  ) %>%
  st_set_geometry(NULL) %>%
  left_join(
    areas %>% select(area_id, geometry),
    by = "area_id"
  ) %>%
  select(survey_region_id, area_id, geometry_area = geometry)

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
    female_age_max = Inf, ## max(if_else(sex == "female", age, NA_integer_), na.rm=TRUE),
    male_age_min = min(if_else(sex == "male", age, NA_integer_), na.rm=TRUE),
    male_age_max = Inf, ## max(if_else(sex == "male", age, NA_integer_), na.rm=TRUE),
    .groups = "drop"
  ) %>%
  mutate(survey_mid_calendar_quarter = survey_mid_calendar_quarter)

survey_sexbehav <- extract_sexbehav_phia(ind, survey_id)
(misallocation <- check_survey_sexbehav(survey_sexbehav))

#' #' ## Calculate indicators by area/sex/age
#'
#' survey_indicators <- calc_survey_hiv_indicators(
#'                               survey_meta,
#'                               survey_regions,
#'                               survey_clusters,
#'                               survey_individuals,
#'                               survey_biomarker,
#'                               as.data.frame(areas)
#'                             )

#' ## Save survey datasets

write_csv(survey_meta, paste0(tolower(survey_id), "_survey_meta.csv"), na = "")
write_csv(survey_regions, paste0(tolower(survey_id), "_survey_regions.csv"), na = "")
write_csv(survey_clusters, paste0(tolower(survey_id), "_survey_clusters.csv"), na = "")
write_csv(survey_individuals, paste0(tolower(survey_id), "_survey_individuals.csv"), na = "")
write_csv(survey_biomarker, paste0(tolower(survey_id), "_survey_biomarker.csv"), na = "")
# write_csv(survey_circumcision, paste0(tolower(survey_id), "_survey_circumcision.csv"), na = "")
write_csv(survey_sexbehav, paste0(tolower(survey_id), "_survey_sexbehav.csv"), na = "")


#' ## Save survey indicators
# write_csv(survey_indicators, paste0(tolower(survey_id), "_hiv_indicators.csv"), na = "")

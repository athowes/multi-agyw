
#' ## Load area hierarchy
orderly_dependency("process_areas",
                   "latest()",
                   "swz_areas.geojson")
areas <- read_sf("swz_areas.geojson")


#' ## Load PHIA datasets
#' Authenticate SharePoint login
#' sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")
#'
#' naomi_raw_path <- "sites/HIVInferenceGroup-WP/Shared Documents/Data/naomi-raw"
#' file <- "SWZ/2022-11-29 SHIMS3 2021 naomi/SHIMS3_2021_unaids_dataset.csv"
#'
#'
#' #' Download files from SharePoint
#' path <- URLencode(file.path(naomi_raw_path, file))
#' phia_path <- sharepoint$download(path)

#' ## Load PHIA datasets
iso3 <- "SWZ"
survey_id  <- "SWZ2021PHIA"
survey_mid_calendar_quarter <- "CY2021Q3"

# phia_col_types <- "cciiiiiiiiiiiiiiddcdd"
#
# phia <- read_csv(phia_path, col_types = phia_col_types) %>%
#   mutate(survey_id = survey_id,
#          cluster_id = CentroidID,
#          household = householdid,
#          individual_id = personid)

phia_path <- "SWZ2021PHIA/datasets"

phia_files <- list(geo = "SHIMS3 2021 Geospatial Data (DTA).zip",
                   survey = "SHIMS3 2021 Household Interview and Biomarker Datasets (DTA).zip") %>%
  lapply(function(x) file.path(phia_path, x))

geo <- rdhs::read_zipdata(phia_files$geo)
names(geo) <- tolower(names(geo))

hh <- rdhs::read_zipdata(phia_files$survey, "shims32021hh.dta")
bio <- rdhs::read_zipdata(phia_files$survey, "shims32021adultbio.dta")
ind <- rdhs::read_zipdata(phia_files$survey, "shims32021adultind.dta")

phia <- ind %>%
  filter(indstatus == 1) %>%  # Respondent
  select(centroidid, region, urban, householdid,
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
#' can be assigned with certainty.

phia$survey_region_id <- phia$region # different in each survey dataset depending on survey stratification

survey_region_id <- c("Hhohho" = 1,
                      "Lubombo" = 2,
                      "Manzini" = 3,
                      "Shiselweni" = 4)


#' Note: In MoH classifications, Mulanje is part of the South-East RegionCode.
#'       In MPHIA, it is allocated to South-West RegionCode. Consequently,
#'       the smallest area that contains the PHIA South-West RegionCode is the
#'       level 1 Southern Region.

survey_region_area_id <- c("Hhohho" = "SWZ_1_1",
                           "Lubombo" = "SWZ_1_2",
                           "Manzini" = "SWZ_1_3",
                           "Shiselweni" = "SWZ_1_4")

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

p_survey_regions <- ggplot(survey_regions) +
  geom_sf(aes(fill = survey_region_name), color = "grey60", alpha = 0.6) +
  geom_sf(data = areas %>% filter(area_level == 1), fill = NA, inherit.aes = FALSE) +
  geom_sf_label(data = areas %>% filter(area_level == 1), aes(label = area_name), inherit.aes = FALSE)

dir.create("check")
ggsave("check/swz-phia-survey-region-boundaries.png", p_survey_regions, h = 7, w = 7)

#' Inspect area_id assigments to confirm

survey_regions %>%
  left_join(
    areas %>%
    as.data.frame() %>%
    select(area_id, area_name, area_level, area_level_label),
    by = c("survey_region_area_id" = "area_id")
  ) %>%
  st_drop_geometry()


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

# mcdetail_labels = c(`1` = "Medical",
#                     `2` = "Non-medical",
#                     `3` = "Uncircumcised",
#                     `4` = "Circumicised but provider unknown",
#                     `5` = "Unknown if circumcised",
#                     `99` = "Missing, including women")
#
# mcdetail_recode = c(`1` = "Healthcare worker",
#                     `2` = "Traditional practitioner",
#                     `3` = NA_character_,
#                     `4` = NA_character_,
#                     `5` = NA_character_,
#                     `99` = NA_character_)
#
# survey_circumcision <- phia %>%
#   filter(gender == 1) %>%
#   transmute(
#     survey_id,
#     individual_id = personid,
#     circumcised = recode(mcdetail, `3` = 0L , `1` = 1L, `2` = 1L, `4` = 1L, .default = NA_integer_),
#     circ_age = recode(as.integer(mcage), `-8` = NA_integer_, `-9` = NA_integer_),
#     circ_where = NA_character_,
#     circ_who = recode(mcdetail, !!!mcdetail_recode)
#   )



#' ## Survey meta data

survey_meta <- survey_individuals %>%
  group_by(survey_id) %>%
  summarise(female_age_min = min(if_else(sex == "female", age, NA_integer_), na.rm=TRUE),
            ## female_age_max = max(if_else(sex == "female", age, NA_integer_), na.rm=TRUE),
            female_age_max = Inf,
            male_age_min = min(if_else(sex == "male", age, NA_integer_), na.rm=TRUE),
            ## male_age_max = max(if_else(sex == "male", age, NA_integer_), na.rm=TRUE),
            male_age_max = Inf) %>%
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
#'
#' #' ## Save survey indicators
#' write_csv(survey_indicators, paste0(tolower(survey_id), "_hiv_indicators.csv"), na = "")

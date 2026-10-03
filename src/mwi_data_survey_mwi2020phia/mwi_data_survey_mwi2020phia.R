# orderly::orderly_develop_start("mwi_survey_phia")
# setwd("src/mwi_survey_phia/")

#' ## Survey meta data

iso3 <- "MWI"
country <- "Malawi"
survey_id  <- "MWI2020PHIA"
survey_mid_calendar_quarter <- "CY2020Q3"
fieldwork_start <- "2020-01-01"
fieldwork_end <- "2021-04-30"

#' ## Load area hierarchy
orderly_dependency("process_areas",
                   "latest()",
                   "mwi_areas.geojson")
areas <- read_sf("mwi_areas.geojson")

#' ## Load PHIA datasets
# sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")
#
# phia_path <- "sites/HIVInferenceGroup-WP/Shared Documents/Data/household surveys/PHIA"
#
# paths <- list(geo = "datasets/MWI/datasets/MPHIA 2015-2016 PR Geospatial Data 20210917.zip",
#               survey = "datasets/MWI/datasets/MPHIA 2015-2016 Household Interview and Biomarker Datasets v2.0 (DTA).zip") %>%
#   lapply(function(x) file.path(phia_path, x)) %>%
#   lapply(URLencode)
#
# phia_files <- lapply(paths, sharepoint$download)

phia_path <- "MWI2020PHIA/datasets"

phia_files <- list(geo = "MPHIA 2020-2021 Geospatial Data (DTA).zip",
              survey = "MPHIA 2020-2021 Household Interview and Biomarker Data (DTA).zip") %>%
  lapply(function(x) file.path(phia_path, x))

geo <- rdhs::read_zipdata(phia_files$geo)

hh <- rdhs::read_zipdata(phia_files$survey, "mphia2020hh.dta")
bio <- rdhs::read_zipdata(phia_files$survey, "mphia2020adultbio.dta")
ind <- rdhs::read_zipdata(phia_files$survey, "mphia2020adultind.dta")

phia <- ind %>%
  filter(indstatus == 1) %>%  # Respondent
  select(centroidid, zone, urban, householdid,
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

hh$survey_region_id <- hh$zone # different in each survey dataset depending on survey stratification

survey_region_id <- c("Northern" = 1,
                      "Central East" = 2,
                      "Central West" = 3,
                      "Lilongwe City" = 4,
                      "South East" = 5,
                      "South West" = 6,
                      "Blantyre City" = 7)

#' Note: In MoH classifications, Mulanje is part of the South-East Zone.
#'       In MPHIA, it is allocated to South-West Zone. Consequently,
#'       the smallest area that contains the PHIA South-West Zone is the
#'       level 1 Southern Region.

survey_region_area_id <- c("Northern" = "MWI_2_1",
                           "Central East" = "MWI_2_2",
                           "Central West" = "MWI_2_3",
                           "Lilongwe City" = "MWI_5_18",
                           "South East" = "MWI_2_4",
                           "South West" = "MWI_1_3",
                           "Blantyre City" = "MWI_5_33")

survey_regions <- tibble(survey_id = survey_id,
                         survey_region_id = survey_region_id,
                         survey_region_name = names(survey_region_id),
                         survey_region_area_id = survey_region_area_id[names(survey_region_id)])

#' Add survey region boundary

survey_regions <- survey_regions %>%
  left_join(
    spread_areas(as.data.frame(areas)) %>%
    left_join(select(areas, area_id), by = "area_id") %>%
    st_as_sf() %>%
    mutate(
      survey_region_name = case_when(area_name3 == "Mulanje" ~ "South West",
                                     area_name5 %in% c("Lilongwe City", "Blantyre City") ~ area_name5,
                                     TRUE ~ sub("(East|West)ern", "\\1", area_name2))
    ) %>%
    group_by(survey_region_name) %>%
    summarise(.groups = "drop"),
    by = "survey_region_name"
  ) %>%
  st_as_sf()


p_check_survey_regions <- ggplot(survey_regions) +
  geom_sf(aes(fill = survey_region_name), color = "grey60", alpha = 0.6) +
  geom_sf(data = areas %>% filter(area_level == 2), fill = NA, inherit.aes = FALSE) +
  ggtitle("Survey regions and health zones (black lines)",
          "Mulange is in South West in MPHIA but South East\nin MoH classification") +
  naomi::th_map()

dir.create("check")
ggsave("check/compare_survey_regions.png", p_check_survey_regions, h=5, w=4)


#' Inspect area_id assigments to confirm

survey_regions %>%
  left_join(
    areas %>%
    as.data.frame() %>%
    select(area_id, area_name, area_level, area_level_label),
    by = c("survey_region_area_id" = "area_id")
  ) %>%
  st_drop_geometry() %>%
  select(survey_region_name, area_name, area_level_label)


#' *** Should not require edits beyond this point ***

#' ## Survey clusters dataset
#'
#' This data frame maps survey clusters to the highest level in the area hiearchy
#' based on geomasked cluster centroids, additionally checking that the geolocated
#' areas are contained in the survey region.

survey_clusters <- hh %>%
  transmute(survey_id = survey_id,
            cluster_id = centroidid,
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
  as.data.frame() %>%
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


#' Create individuals data

survey_individuals <-
  bind_rows(
    phia %>%
    transmute(
      survey_id,
      cluster_id,
      individual_id = personid,
      household = householdid,
      line = personid,
      interview_cmc = 12 * (surveystyear - 1900) + surveystmonth,
      sex = factor(gender, 1:2, c("male", "female")),
      age,
      dob_cmc = NA,
      indweight = intwt0
    )
  ) %>%
  mutate(age = as.integer(age),
         indweight = indweight / mean(indweight, na.rm=TRUE))


survey_biomarker <-
  bind_rows(
    phia %>%
    filter(!is.na(hivstatusfinal)) %>%
    transmute(
      survey_id,
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
      cd4 = cd4count,
      recent = case_when(recentlagvlarv == 1 ~ 1,
                         recentlagvlarv == 2 ~ 0)
    )
  ) %>%
  mutate(hivweight = hivweight / mean(hivweight, na.rm = TRUE))




survey_meta <- survey_individuals %>%
  group_by(survey_id) %>%
  summarise(female_age_min = min(if_else(sex == "female", age, NA_integer_), na.rm=TRUE),
            female_age_max = max(if_else(sex == "female", age, NA_integer_), na.rm=TRUE),
            male_age_min = min(if_else(sex == "male", age, NA_integer_), na.rm=TRUE),
            male_age_max = max(if_else(sex == "male", age, NA_integer_), na.rm=TRUE),
            .groups = "drop") %>%
  mutate(iso3 = substr(survey_id, 1, 3),
         country = country,
         survey_type = "PHIA",
         survey_mid_calendar_quarter = recode(iso3, "MWI" = survey_mid_calendar_quarter),
         fieldwork_start = fieldwork_start,
         fieldwork_end   = fieldwork_end)

survey_sexbehav <- extract_sexbehav_phia(ind, survey_id)
(misallocation <- check_survey_sexbehav(survey_sexbehav))

#' ## Save survey datasets

write_csv(survey_meta, paste0(tolower(survey_id), "_survey_meta.csv"), na = "")
write_csv(survey_regions, paste0(tolower(survey_id), "_survey_regions.csv"), na = "")
write_csv(survey_clusters, paste0(tolower(survey_id), "_survey_clusters.csv"), na = "")
write_csv(survey_individuals, paste0(tolower(survey_id), "_survey_individuals.csv"), na = "")
write_csv(survey_sexbehav, paste0(tolower(survey_id), "_survey_sexbehav.csv"), na = "")

write_csv(survey_biomarker, paste0(tolower(survey_id), "_survey_biomarker.csv"), na = "")
write_csv(survey_circumcision, paste0(tolower(survey_id), "_survey_circumcision.csv"), na = "")

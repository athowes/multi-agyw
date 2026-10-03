# orderly::orderly_develop_start("gmb_survey_mics2018")
# setwd("src/gmb_survey_mics2018/")

#' ## Survey meta data

iso3 <- "GMB"
country <- "Gambia"
survey_id  <- "GMB2018MICS"
survey_mid_calendar_quarter <- "CY2018Q3"
fieldwork_start <- "2018-01-01"
fieldwork_end <- "2018-12-31"

#' ## Load area hierarchy
orderly_dependency("process_areas",
                   "latest()",
                   "gmb_areas.geojson")
areas <- read_sf("gmb_areas.geojson")

geo <- readr::read_csv("GMB2018MICS.csv")

# geo coordinates missing for some of our clusters, causes problems so dropping for now
missing_geos <- geo$cluster_id[is.na(geo$latitude)]

# re-link to current areas so drop what's imported here
geo <- geo %>%
  select(cluster_id, longitude, latitude) %>%
  filter(!is.na(longitude) & !is.na(latitude)) # 16 clusters have missing geographic coords
                                               # causes problems, dropping them

hh <- readRDS("GMB2018MICS.rds")$hh
ind <- readRDS("GMB2018MICS.rds")$wm
names(hh) <- tolower(names(hh))
names(ind) <- tolower(names(ind))

# geo coordinates missing for some of our clusters, causes problems so dropping for now
hh <- hh %>%
  filter(!hh1 %in% missing_geos)
ind <- ind %>%
  filter(!wm1 %in% missing_geos)

mics <- ind %>%
  filter(wm17 == 1) %>% # filter out anyone who didn't complete the survey
  select(wm1, wm2, wm3, wm6y, wm6m, # DO I NEED URBAN AND REGION IN HERE?  Only in HH
         psu, wmweight, stratum,wb4,
         ethnicity #, religion # no ethnicity or religion
  )  %>%
  mutate( personid = paste0(wm1,"_",wm2,"_",wm3),
          gender = "female",
          survey_id = survey_id,
          cluster_id = wm1)




#' ## Survey regions
#'
#' This table identifies the smallest area in the area hierarchy which contains each
#' region in the survey stratification, which is the smallest area to which a cluster
#' can be assigned with certainty.

hh$survey_region_id <- hh$hh7 # different in each survey dataset depending on survey stratification

survey_region_id <- c("Banjul" = 1,
                      "Kanifing" = 2,
                      "Brikama" = 3,
                      "Mansakonko" = 4,
                      "Kerewan" = 5,
                      "Kuntaur" = 6,
                      "Janjanbureh" = 7,
                      "Basse" = 8)

areas %>% filter(area_level == 1) %>% select(area_id, area_name)


survey_region_area_id <- c("Banjul" = "GMB_1_5mz",
                           "Kanifing" = "GMB_1_5mz",
                           "Brikama" = "GMB_1_5mz",
                           "Mansakonko" = "GMB_1_2xt",
                           "Kerewan" = "GMB_1_3ic",
                           "Kuntaur" = "GMB_1_1ng",
                           "Janjanbureh" = "GMB_1_1ng",
                           "Basse" = "GMB_1_4ui")

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

survey_clusters <- hh %>%
  transmute(survey_id,
            cluster_id = hh1,
            res_type = factor(hh6, 1:2, c("urban", "rural")),
            survey_region_id) %>%
  distinct() %>%
  left_join(geo, by = c("cluster_id")) %>%
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

survey_individuals <- mics %>%
  transmute(
    survey_id,
    cluster_id,
    individual_id = personid,
    household = wm2,
    line = wm3,
    interview_cmc = 12 * (wm6y - 1900) + wm6m,
    sex = factor(gender, levels=c("male", "female")),
    age = wb4,
    dob_cmc = NA,
    indweight = wmweight
  ) %>%
  mutate(age = as.integer(age),
         indweight = indweight / mean(indweight, na.rm=TRUE))




survey_meta <- survey_individuals %>%
  group_by(survey_id) %>%
  summarise(female_age_min = min(if_else(sex == "female", age, NA_integer_), na.rm=TRUE),
            female_age_max = max(if_else(sex == "female", age, NA_integer_), na.rm=TRUE),
            male_age_min = min(if_else(sex == "male", age, NA_integer_), na.rm=TRUE),
            male_age_max = max(if_else(sex == "male", age, NA_integer_), na.rm=TRUE),
            .groups = "drop") %>%
  mutate(iso3 = substr(survey_id, 1, 3),
         country = country,
         survey_type = "MICS",
         survey_mid_calendar_quarter = recode(iso3, "GMB" = survey_mid_calendar_quarter),
         fieldwork_start = fieldwork_start,
         fieldwork_end   = fieldwork_end)

survey_sexbehav <- extract_sexbehav_mics(ind, survey_id, gender="female")
(misallocation <- check_survey_sexbehav(survey_sexbehav))

#' ## Save survey datasets

write_csv(survey_meta, paste0(tolower(survey_id), "_survey_meta.csv"), na = "")
write_csv(survey_regions, paste0(tolower(survey_id), "_survey_regions.csv"), na = "")
write_csv(survey_clusters, paste0(tolower(survey_id), "_survey_clusters.csv"), na = "")
write_csv(survey_individuals, paste0(tolower(survey_id), "_survey_individuals.csv"), na = "")
write_csv(survey_sexbehav, paste0(tolower(survey_id), "_survey_sexbehav.csv"), na = "")


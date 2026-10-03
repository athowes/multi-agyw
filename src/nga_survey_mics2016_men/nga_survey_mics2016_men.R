# orderly::orderly_develop_start("gmb_survey_mics2018")
# setwd("src/gmb_survey_mics2018/")

#### CODING DIFFERENT FROM OTHER MICS - THIS IS MICS 5

#' ## Survey meta data

iso3 <- "NGA"
country <- "Nigeria"
survey_id  <- "NGA2016MICS"
survey_mid_calendar_quarter <- "CY2016Q4"
fieldwork_start <- "2016-01-01"
fieldwork_end <- "2017-12-31"

#' ## Load area hierarchy
orderly_dependency("process_areas",
                   "latest()",
                   "nga_areas.geojson")
areas <- read_sf("nga_areas.geojson")

geo <- readr::read_csv("NGA2016MICS.csv")

# geo coordinates missing for some of our clusters, causes problems so dropping for now
missing_geos <- geo$cluster_id[is.na(geo$latitude)]

# re-link to current areas so drop what's imported here
geo <- geo %>%
  select(cluster_id, longitude, latitude) %>%
  filter(!is.na(longitude) & !is.na(latitude)) # 16 clusters have missing geographic coords
                                               # causes problems, dropping them

hh <- readRDS("NGA2016MICS.rds")$hh
ind <- readRDS("NGA2016MICS.rds")$mn
names(hh) <- tolower(names(hh))
names(ind) <- tolower(names(ind))

# geo coordinates missing for some of our clusters, causes problems so dropping for now
hh <- hh %>%
  filter(!hh1 %in% missing_geos)
ind <- ind %>%
  filter(!mwm1 %in% missing_geos)

#### FIX SEXUAL BEHAVIOR CODING TO MATCH MICS 6
ind <- ind %>%
  select(-msb4,-msb9) %>% # drop inconsistently labeled vars we don't need
  rename(msb2u = msb3u,
         msb2n = msb3n,
         msb4 = msb5,
         msb7 = msb8,
         msb9 = msb10) %>%
  mutate(mwm3 = ln)

mics <- ind %>%
  filter(mwm7 == 1) %>% # filter out anyone who didn't complete the survey
  select(mwm1, mwm2, ln, mwm6y, mwm6m, # DO I NEED URBAN AND REGION IN HERE?  Only in HH
         #psu,
         mnweight, #stratum,
         mwb2 # ,
         #ethnicity #, religion # no ethnicity or religion
  )  %>%
  mutate( personid = paste0(mwm1,"_",mwm2,"_",ln),
          gender = "male",
          survey_id = survey_id,
          cluster_id = mwm1)




#' ## Survey regions
#'
#' This table identifies the smallest area in the area hierarchy which contains each
#' region in the survey stratification, which is the smallest area to which a cluster
#' can be assigned with certainty.

hh$survey_region_id <- hh$hh7 # different in each survey dataset depending on survey stratification

survey_region_id <- c(     "Abia" = 1,
                           "Adamawa" = 2,
                           "Akwa Ibom" = 3,
                           "Anambra" = 4,
                           "Bauchi" = 5,
                           "Bayelsa" = 6,
                           "Benue" = 7,
                           "Borno" = 8,
                            "Cross River" = 9,
                                  "Delta" = 10,
                                 "Ebonyi" = 11,
                                    "Edo" = 12,
                                  "Ekiti" = 13,
                                  "Enugu" = 14,
                                  "Gombe" = 15,
                                    "Imo" = 16,
                                 "Jigawa" = 17,
                                 "Kaduna" = 18,
                                   "Kano" = 19,
                                "Katsina" = 20,
                                  "Kebbi" = 21,
                                   "Kogi" = 22,
                                  "Kwara" = 23,
                                 "Lagos" = 24,
                               "Nasarawa" = 25,
                                  "Niger" = 26,
                                   "Ogun" = 27,
                                   "Ondo" = 28,
                                   "Osun" = 29,
                                    "Oyo" = 30,
                                "Plateau" = 31,
                                 "Rivers" = 32,
                                 "Sokoto" = 33,
                                 "Taraba" = 34,
                                   "Yobe" = 35,
                                "Zamfara" = 36,
                              "FCT Abuja" = 37)

areas %>% filter(area_level == 2) %>% select(area_id, area_name)
# Districts went from 14 to 16 in 2017 - MICS is using pre-2017


survey_region_area_id <- c(     "Abia" = "NGA_2_AB",
                                "Adamawa" = "NGA_2_AD",
                                "Akwa Ibom" = "NGA_2_AK",
                                "Anambra" = "NGA_2_AN",
                                "Bauchi" = "NGA_2_BA",
                                "Bayelsa" = "NGA_2_BY",
                                "Benue" = "NGA_2_BE",
                                "Borno" = "NGA_2_BR",
                                "Cross River" = "NGA_2_CR",
                                "Delta" = "NGA_2_DE",
                                "Ebonyi" = "NGA_2_EB",
                                "Edo" = "NGA_2_ED",
                                "Ekiti" = "NGA_2_EK",
                                "Enugu" = "NGA_2_EN",
                                "Gombe" = "NGA_2_GO",
                                "Imo" = "NGA_2_IM",
                                "Jigawa" = "NGA_2_JI",
                                "Kaduna" = "NGA_2_KD",
                                "Kano" = "NGA_2_KN",
                                "Katsina" = "NGA_2_KT",
                                "Kebbi" = "NGA_2_KB",
                                "Kogi" = "NGA_2_KO",
                                "Kwara" = "NGA_2_KW",
                                "Lagos" = "NGA_2_LA",
                                "Nasarawa" = "NGA_2_NA",
                                "Niger" = "NGA_2_NI",
                                "Ogun" = "NGA_2_OG",
                                "Ondo" = "NGA_2_ON",
                                "Osun" = "NGA_2_OS",
                                "Oyo" = "NGA_2_OY",
                                "Plateau" = "NGA_2_PL",
                                "Rivers" = "NGA_2_RI",
                                "Sokoto" = "NGA_2_SO",
                                "Taraba" = "NGA_2_TA",
                                "Yobe" = "NGA_2_YO",
                                "Zamfara" = "NGA_2_ZA",
                                "FCT Abuja" = "NGA_2_FC")

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
    household = mwm2,
    line = ln,
    interview_cmc = 12 * (mwm6y - 1900) + mwm6m,
    sex = factor(gender, levels=c("male", "female")),
    age = mwb2,
    dob_cmc = NA,
    indweight = mnweight
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
         survey_mid_calendar_quarter = recode(iso3, "NGA" = survey_mid_calendar_quarter),
         fieldwork_start = fieldwork_start,
         fieldwork_end   = fieldwork_end)

survey_sexbehav <- extract_sexbehav_mics(ind, survey_id, gender="male")
(misallocation <- check_survey_sexbehav(survey_sexbehav))

#' ## Save survey datasets

write_csv(survey_meta, paste0(tolower(survey_id), "_survey_meta.csv"), na = "")
write_csv(survey_regions, paste0(tolower(survey_id), "_survey_regions.csv"), na = "")
write_csv(survey_clusters, paste0(tolower(survey_id), "_survey_clusters.csv"), na = "")
write_csv(survey_individuals, paste0(tolower(survey_id), "_survey_individuals.csv"), na = "")
write_csv(survey_sexbehav, paste0(tolower(survey_id), "_survey_sexbehav.csv"), na = "")


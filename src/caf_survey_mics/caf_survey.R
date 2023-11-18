# orderly::orderly_develop_start("caf_survey_mics")
# setwd("src/caf_survey_mics/")

#' ## Survey meta data

iso3 <- "CAF"
country <- "Central African Republic"
survey_id  <- "CAF2018MICS"
survey_mid_calendar_quarter <- "CY2018Q4"

#' ## Load area hierarchy
areas <- read_sf("depends/caf_areas.geojson")

#' #' Authenticate SharePoint login
#' sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")
#'
#' naomi_raw_path <- "sites/HIVInferenceGroup-WP/Shared Documents/Data/household surveys/MICS/automate/zip"
#' file_path <- "Central African Republic MICS6 Datasets.zip"
#' url <- URLencode(file.path(naomi_raw_path, file_path))
#' mics <- sharepoint$download(url)
#'
#' mics_dat <- unzip(mics, c("Central African Republic MICS6 SPSS Datasets/wm.sav",
#'                           "Central African Republic MICS6 SPSS Datasets/hh.sav"))
#'

mics_dat <- unzip("Central African Republic MICS6 Datasets.zip",
                  c("Central African Republic MICS6 Datasets/Central African Republic MICS6 SPSS Datasets/wm.sav",
                          "Central African Republic MICS6 Datasets/Central African Republic MICS6 SPSS Datasets/hh.sav"))

ind <- haven::read_sav(mics_dat[1])
hh <- haven::read_sav(mics_dat[2])

mics <- ind %>%
  filter(WM17 == 1) %>% # filter out anyone who didn't complete the survey
  select(WM1, WM2, WM3, WM6Y, WM6M, # DO I NEED URBAN AND REGION IN HERE?  Only in HH
         PSU, wmweight, stratum,WB4,
         ethnicity, religion)  %>%
  mutate( personid = paste0(WM1,"_",WM2,"_",WM3),
          gender = "female",
          survey_id = survey_id,
          cluster_id = WM1)

#' ## Survey regions
#'
#' This table identifies the smallest area in the area hierarchy which contains each
#' region in the survey stratification, which is the smallest area to which a cluster
#' can be assigned with certainty. (Usually admin 1)

hh$survey_region_id <- hh$HH7 # different in each survey dataset depending on survey stratification

survey_region_id <- c("RS1" = 1,
                      "RS2" = 2,
                      "RS3" = 3,
                      "RS4" = 4,
                      "RS5" = 5,
                      "RS6" = 6,
                      "RS7" = 7)

areas %>%
  filter(area_level == 1) %>%
  select(area_id, area_name) %>%
  st_drop_geometry() %>%
  print(n = Inf)

survey_regions <- tibble(survey_id = survey_id,
                         survey_region_id = survey_region_id,
                         survey_region_name = names(survey_region_id)) %>%
  left_join(
    areas %>%
      filter(area_level == 1) %>%
      select(survey_region_area_id = area_id, survey_region_name = area_name)
  ) %>% st_as_sf()


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
  transmute(survey_id = survey_id,
            cluster_id = HH1,
            cluster_id,
            res_type = factor(HH6, 1:2, c("urban", "rural")),
            survey_region_id) %>%
  distinct() %>%
  left_join(tibble(survey_id = survey_id,
                   survey_region_id = survey_region_id,
                   survey_region_name = names(survey_region_id))) %>%
  left_join(
    areas %>%
      filter(area_level == 1) %>%
      st_drop_geometry() %>%
      select(survey_region_name = area_name, latitude = center_x, longitude = center_y)
  ) %>%
  select(-survey_region_name) %>%
  sf::st_as_sf(coords = c("latitude", "longitude"), remove = FALSE) %>%
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

#' ## Survey individuals dataset

#' Create individuals data

survey_individuals <-
    mics %>%
      transmute(
        survey_id,
        cluster_id,
        individual_id = personid,
        household = WM2,
        line = WM3,
        interview_cmc = 12 * (WM6Y - 1900) + WM6M,
        sex = factor(gender, levels=c("male", "female")),
        age = WB4,
        dob_cmc = NA,
        indweight = wmweight
      ) %>%
  mutate(age = as.integer(age))


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
         survey_mid_calendar_quarter = recode(iso3, "CAF" = survey_mid_calendar_quarter),
         fieldwork_start = NA,
         fieldwork_end   = NA)

survey_sexbehav <- extract_sexbehav_mics(ind, survey_id, gender="female")
(misallocation <- check_survey_sexbehav(survey_sexbehav))

#' ## Save survey datasets

write_csv(survey_meta, paste0(tolower(survey_id), "_survey_meta.csv"), na = "")
write_csv(survey_regions, paste0(tolower(survey_id), "_survey_regions.csv"), na = "")
write_csv(survey_clusters, paste0(tolower(survey_id), "_survey_clusters.csv"), na = "")
write_csv(survey_individuals, paste0(tolower(survey_id), "_survey_individuals.csv"), na = "")
write_csv(survey_sexbehav, paste0(tolower(survey_id), "_survey_sexbehav.csv"), na = "")

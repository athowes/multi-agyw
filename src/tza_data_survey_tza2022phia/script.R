# orderly::orderly_develop_start("tza_survey_phia")
# setwd("src/tza_survey_phia/")

#' ## Survey meta data

#' ## Load PHIA datasets
iso3 <- "TZA"
country <- "Tanzania"
survey_id  <- "TZA2022PHIA"
survey_mid_calendar_quarter <- "CY2022Q4"
fieldwork_start <- NA
fieldwork_end <- NA

phia_col_types <- "cciiiiiiiiiiiiiiiiiiiiddcdd"

#' ## Load area hierarchy
areas <- read_sf("depends/tza_areas.geojson")

#' #' ## Load PHIA datasets
#' sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")
#'
#' naomi_raw_path <- "sites/HIVInferenceGroup-WP/Shared%20Documents/Data/naomi-raw"
#' file <- "TZA/2023-12-06%20TZA2022PHIA/THIS2%202022%20UNAIDS%20Georeferenced%20Dataset/THIS2_2022_unaids_dataset.csv"
#'
#' #' Download files from SharePoint
#' path <- URLencode(file.path(naomi_raw_path, file))
#' phia_path <- sharepoint$download(path)

phia_path <- "TZA/THIS2_2022_unaids_dataset2.csv"

phia <- read_csv(phia_path, col_types = phia_col_types) %>%
  mutate(survey_id = survey_id,
         cluster_id = CentroidID,
         individual_id = personid,
         household = householdid,
         line = personid)

#' ## Survey regions
#'
#' This table identifies the smallest area in the area hierarchy which contains each
#' region in the survey stratification, which is the smallest area to which a cluster
#' can be assigned with certainty. (Usually admin 1)

phia$survey_region_id <- phia$region # different in each survey dataset depending on survey stratification

survey_region_id <- c("Dodoma" = 1,
                      "Arusha" = 2,
                      "Kilimanjaro" = 3, "Tanga" = 4,
                      "Morogoro" = 5, "Pwani" = 6, "Dar-es-salaam" = 7, "Lindi" = 8,
                      "Mtwara" = 9, "Ruvuma" = 10, "Iringa" = 11, "Mbeya" = 12,
                      "Singida" = 13, "Tabora" = 14, "Rukwa" = 15, "Kigoma" = 16,
                      "Shinyanga" = 17, "Kagera" = 18, "Mwanza" = 19, "Mara" = 20,
                      "Manyara" = 21, "Njombe" = 22, "Katavi" = 23, "Simiyu" = 24,
                      "Geita" = 25, "Songwe" = 26,
                      "Kaskazini Unguja" = 51, "Kusini Unguja" = 52,
                      "Mjini Magharibi" = 53, "Kaskazini Pemba" = 54,
                      "Kusini Pemba" = 55)

survey_regions <- areas %>%
  filter(area_level == 3) %>%
  select(survey_region_area_id = area_id,
         survey_region_name = area_name) %>%
  full_join(
    tibble(survey_id = survey_id,
           survey_region_name = names(survey_region_id),
           survey_region_id = survey_region_id)
  ) %>%
  select(survey_id, survey_region_id, survey_region_name, survey_region_area_id)

p_survey_regions <- ggplot(survey_regions) +
  geom_sf(aes(fill = survey_region_name), color = "grey60", alpha = 0.6) +
  geom_sf(data = areas %>% filter(area_level == 1), fill = NA, inherit.aes = FALSE)

dir.create("check")
ggsave("check/tza2022phia-survey-region-boundaries.png", p_survey_regions, h = 7, w = 7)


#' Inspect area_id assigments to confirm

survey_regions %>%
  left_join(
    areas %>%
      as.data.frame() %>%
      select(area_id, area_name, area_level, area_level_label),
    by = c("survey_region_area_id" = "area_id")
  ) %>%
  print(n = Inf)


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
            survey_region_id,
            longitude = longitude,
            latitude = latitude) %>%
  distinct() %>%
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
    individual_id,
    household,
    line,
    interview_cmc = NA,
    sex = factor(gender, 1:2, c("male", "female")),
    age,
    dob_cmc = NA,
    indweight = intwt0
  )

survey_biomarker <- phia %>%
  group_by(survey_id) %>%  # for normalizing weights
  transmute(
    individual_id,
    hivweight = btwt0 / mean(btwt0),
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
  ungroup()


#' ## Survey meta data

survey_meta <- survey_individuals %>%
  group_by(survey_id) %>%
  summarise(female_age_min = min(if_else(sex == "female", age, NA_integer_), na.rm=TRUE),
            female_age_max = max(if_else(sex == "female", age, NA_integer_), na.rm=TRUE),
            male_age_min = min(if_else(sex == "male", age, NA_integer_), na.rm=TRUE),
            male_age_max = max(if_else(sex == "male", age, NA_integer_), na.rm=TRUE)) %>%
  mutate(iso3 = substr(survey_id, 1, 3),
         country = country,
         survey_type = "PHIA",
         survey_mid_calendar_quarter = survey_mid_calendar_quarter,
         fieldwork_start = fieldwork_start,
         fieldwork_end   = fieldwork_end) %>%
  ungroup()

survey_sexbehav <- extract_sexbehav_phia(phia, survey_id)
(misallocation <- check_survey_sexbehav(survey_sexbehav))

#' ## Save survey datasets

write_csv(survey_meta, paste0(tolower(survey_id), "_survey_meta.csv"), na = "")
write_csv(survey_regions, paste0(tolower(survey_id), "_survey_regions.csv"), na = "")
write_csv(survey_clusters, paste0(tolower(survey_id), "_survey_clusters.csv"), na = "")
write_csv(survey_individuals, paste0(tolower(survey_id), "_survey_individuals.csv"), na = "")
write_csv(survey_biomarker, paste0(tolower(survey_id), "_survey_biomarker.csv"), na = "")
write_csv(survey_sexbehav, paste0(tolower(survey_id), "_survey_sexbehav.csv"), na = "")

## sf v 1.0.0 update changes to use s2 spherical geometry as default
## This creates issues for DHS coordinate data extraction scripts
## Revert back to planar geometry
sf::sf_use_s2(FALSE)


#' ## Load area hierarchy
areas <- read_sf("depends/naomi_areas.geojson") %>%
  st_make_valid()


#' Authenticate SharePoint login
sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")

#' Read in files from SharePoint
raw_path <- "sites/HIVInferenceGroup-WP/Shared%20Documents/Data/naomi-raw/ETH/2019-12-05/EPHIA_2017_UNAIDS_DATASET_20191030.csv"
raw_path <- URLencode(raw_path)

phia_path <- sharepoint$download(raw_path)


#' ## Load PHIA datasets
survey_id  <- "ETH2017PHIA"
survey_year <- 2017
survey_mid_calendar_quarter <- "CY2017Q4"

phia_col_types <- "cciiiiiiiiiiiiddcdd"

phia <- read_csv(phia_path, col_types = phia_col_types) %>%
  mutate(survey_id = survey_id,
         cluster_id = paste(varstrat, varunit),
         individual_id = hhid_ln,
         household = hhid,
         line = hhid_ln)

#' ## Survey regions
#'
#' This table identifies the smallest area in the area hierarchy which contains each
#' region in the survey stratification, which is the smallest area to which a cluster
#' can be assigned with certainty. (Usually admin 1)

phia$survey_region_id <- phia$RegionCode # different in each survey dataset depending on survey stratification

survey_region_id <- c("Tigray" = 1,
                      "Afar" = 2,
                      "Amhara" = 3,
                      "Oromia" = 4,
                      "Somali" = 5,
                      "Benishangul Gumuz" = 6,
                      "SNNPR" = 7,
                      "Gambella" = 8,
                      "Harari" = 9,
                      "Addis Ababa" = 10,
                      "Dire Dawa" = 11)

areas %>% filter(area_level == 1) %>% select(area_id, area_name)

#' SNNPR is survey_region_area_id = "ETH" after split of SNNPR and Sidama
#' regions

survey_region_area_id <- c("Tigray" = "ETH_01_20",
                           "Afar" = "ETH_01_11",
                           "Amhara" = "ETH_01_12",
                           "Oromia" = "ETH_01_17",
                           "Somali" = "ETH_01_19",
                           "Benishangul Gumuz" = "ETH_01_13",
                           "SNNPR" = "ETH",
                           "Gambella" = "ETH_01_15",
                           "Harari" = "ETH_01_16",
                           "Addis Ababa" = "ETH_01_10",
                           "Dire Dawa" = "ETH_01_14")
# 2023 version
# survey_region_area_id <- c("Tigray" = "ETH_1_20",
#                            "Afar" = "ETH_1_11",
#                            "Amhara" = "ETH_1_12",
#                            "Oromia" = "ETH_1_17",
#                            "Somali" = "ETH_1_19",
#                            "Benishangul Gumuz" = "ETH_1_13",
#                            "SNNPR" = "ETH",
#                            "Gambella" = "ETH_1_15",
#                            "Harari" = "ETH_1_16",
#                            "Addis Ababa" = "ETH_1_10",
#                            "Dire Dawa" = "ETH_1_14")

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
            survey_region_id,
            longitude = Longitude,
            latitude = Latitude) %>%
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

#' Calculate distance to each candidate area for each cluster

survey_clusters <- survey_clusters %>%
  left_join(survey_region_areas, by = "survey_region_id") %>%
  mutate(
    distance = unlist(Map(sf::st_distance, geometry, geometry_area))
  )

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
    indweight = NA
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
  ungroup


#' ## Survey meta data

survey_meta <- survey_individuals %>%
  group_by(survey_id) %>%
  summarise(female_age_min = min(if_else(sex == "female", age, NA_integer_), na.rm=TRUE),
            female_age_max = max(if_else(sex == "female", age, NA_integer_), na.rm=TRUE),
            male_age_min = min(if_else(sex == "male", age, NA_integer_), na.rm=TRUE),
            male_age_max = max(if_else(sex == "male", age, NA_integer_), na.rm=TRUE)) %>%
  mutate(survey_mid_calendar_quarter = survey_mid_calendar_quarter) %>%
  ungroup()

#' ## Calculate indicators by area/sex/age

survey_indicators <- calc_survey_hiv_indicators(
                              survey_meta,
                              survey_regions,
                              survey_clusters,
                              survey_individuals,
                              survey_biomarker,
                              as.data.frame(areas),
                              by_res_type = TRUE
                            )

#' ETH survey was urban only
survey_indicators <- survey_indicators %>%
  filter(res_type == "urban")

#' ## Save survey indicators


write_csv(survey_indicators, "eth2017phia_hiv_indicators.csv", na = "")


#' ## Save survey regions and survey clusters dataset

write_csv(survey_regions, "eth2017phia_survey_regions.csv", na = "")
write_csv(survey_clusters, "eth2017phia_survey_clusters.csv", na = "")


## Visualise EPHIA clusters

p <- areas %>%
  st_drop_geometry() %>%
  naomi::spread_areas() %>%
  left_join(select(areas, center_x, center_y, area_id)) %>%
  st_as_sf() %>%
  left_join(
    count(survey_clusters, area_id = geoloc_area_id, name = "n_clusters")
  ) %>%
  ggplot() +
  geom_sf(aes(fill = n_clusters > 0)) +
  geom_point(aes(center_x, center_y, size = n_clusters),
             color = "darkred", alpha = 0.6) +
  scale_size_area() +
  scale_fill_manual(values = "grey70", na.value = "grey90", guide = FALSE) +
  ggtitle("EPHIA: Geomasked clusters by Zone") +
  naomi::th_map() +
  labs(x = NULL, y = NULL)

ggsave("ephia-clusters-per-zone.png",p,  h = 4, w=5)

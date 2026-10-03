#' ## Load area hierarchy
orderly_dependency("process_areas",
                   "latest()",
                   "zmb_areas.geojson")
areas <- read_sf("zmb_areas.geojson")

#' #' Authenticate SharePoint login
#' sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")
#'
#' naomi_raw_path <- "sites/HIVInferenceGroup-WP/Shared Documents/Data/naomi-raw/"

# urls <- list(phia_path = "ZMB/2022-12-06 ZamPHIA 2021/zamphia_spectrum_data_20221211.zip",
#              gadm1 = "ZMB/2019-06-25/gadm36_ZMB_1_sf.rds"
#              ) %>%
#   lapply(function(x) file.path(naomi_raw_path, x)) %>%
#   lapply(URLencode)
#
# files <- lapply(urls, sharepoint$download)

phia_path <- "ZMB2021PHIA"

phia_files <- list(geo = "ZAMPHIA 2021 Geospatial Data and Documentation.zip",
                   survey = "ZAMPHIA 2021 PU Package.zip") %>%
  lapply(function(x) file.path(phia_path, x))

geo <- rdhs::read_zipdata(phia_files$geo)

hh <- rdhs::read_zipdata(phia_files$survey, "zamphia2021hh.dta")
bio <- rdhs::read_zipdata(phia_files$survey, "zamphia2021adultbio.dta")
ind <- rdhs::read_zipdata(phia_files$survey, "zamphia2021adultind.dta")

#' ## Load PHIA datasets
iso3 <- "ZMB"
survey_id <- "ZMB2021PHIA"
survey_mid_calendar_quarter <- "CY2021Q3"

# tmpd <- tempfile()
# unzip(files$phia_path, exdir = tmpd)
#
# phia_raw <- read_excel(file.path(tmpd, "zamphia2021_spectrum_data2.xlsx"))

phia <- ind %>%
  filter(indstatus == 1) %>%  # Respondent
  select(centroidid, hprovince_za, urban, householdid,
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
         cluster_id = centroidid) %>%
  rename(region = hprovince_za)

#' ## Survey regions
#'
#' This table identifies the smallest area in the area hierarchy which contains each
#' region in the survey stratification, which is the smallest area to which a cluster
#' can be assigned with certainty. (Usually admin 1)

phia$survey_region_id <- phia$region # different in each survey dataset depending on survey stratification

survey_region_id <- c("Central" = 1,
                      "Copperbelt" = 2,
                      "Eastern" = 3,
                      "Luapula" = 4,
                      "Lusaka" = 5,
                      "Muchinga" = 6,
                      "Northern" = 7,
                      "North-Western" = 8,
                      "Southern" = 9,
                      "Western" = 10)


areas %>% filter(area_level == 1) %>% select(area_id, area_name)

#' Note: ZAMPHIA stratification used old province boundaries from GADM.
#'  Since then:
#'  * Itezhi-tezhi district moved from Southern province to Central province
#'  * Chirundu district moved from Southern province to Lusaka.
#'  * Shibuyunji district was moved in 2018 from Lusaka to Central. But in the PHIA boundaries, Shibuyunji
#'    area was in Central (not existing as district per GADM level 2). Thus no net change for this district.
#'
#' Updates for 2023 estimates:
#' * Itezhi-Tezhi has moved from Central province to Southern province [RETURN TO ORIGINAL PROVINCE]
#' * Chirundu moved from Lusaka to Southern province [RETURN TO ORIGINAL PROVINCE]
#' * Chama moved from Muchinga to Eastern province [NEW CHANGE]
#' * Shibuyunji district was moved in 2018 from Lusaka to Central (That is correct)


zmb_gadm1 <- readRDS("gadm36_ZMB_1_sf.rds")

p1 <- ggplot() +
  geom_sf(aes(fill = NAME_1),
          data = zmb_gadm1 %>%
            filter(NAME_1 %in% c("Central", "Southern", "Lusaka", "Muchinga", "Eastern")),
          alpha = 0.6, color = NA) +
  geom_sf(data = areas %>% filter(area_level == 2), fill = NA, color = "grey") +
  geom_sf_label(aes(label = area_name),
                data = areas %>%
                  filter(area_level == 2,
                         area_name %in% c("Itezhi-tezhi", "Shibuyunji", "Chirundu", "Chama")),
                alpha = 0.3) +
  geom_sf(data = areas %>% filter(area_level == 1), fill = NA) +
  labs(fill = "ZAMPHIA strata") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = c(0.05, 0.95),
        legend.just = c(0, 1))

dir.create("check")
ggsave("check/zambia-reallocated-districts.pdf", p1, h = 6, w = 7)

survey_region_area_id <- c("Central" = "ZMB_1_10gh",
                           "Copperbelt" = "ZMB_1_11",
                           "Eastern" = "ZMB",
                           "Luapula" = "ZMB_1_13",
                           "Lusaka" = "ZMB_1_14fz",
                           "Muchinga" = "ZMB_1_15pi",
                           "Northern" = "ZMB_1_16",
                           "North-Western" = "ZMB_1_19",
                           "Southern" = "ZMB_1_17xt",
                           "Western" = "ZMB_1_18")

survey_regions <- tibble(survey_id = survey_id,
                         survey_region_id = survey_region_id,
                         survey_region_name = names(survey_region_id),
                         survey_region_area_id = survey_region_area_id[names(survey_region_id)])


#' Add survey region boundary

survey_regions <- survey_regions %>%
  left_join(
    spread_areas(as.data.frame(areas)) %>%
    left_join(select(areas, area_id)) %>%
    st_as_sf() %>%
    mutate(survey_region_name = case_when(area_name2 == "Chama" ~ "Muchinga",
                                          area_name2 == "Itezhi-tezhi" ~ "Central",
                                          TRUE ~ area_name1)) %>%
    group_by(survey_region_name) %>%
    summarise()
  ) %>%
  st_as_sf()

p2 <- ggplot(survey_regions) +
  geom_sf(aes(fill = survey_region_name), color = "grey60", alpha = 0.6) +
  geom_sf(data = areas %>% filter(area_level == 1), fill = NA, inherit.aes = FALSE) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

ggsave("check/zamphia-survey-regions.pdf", p2, h = 5, w = 7)




#' Inspect area_id assigments to confirm

survey_regions %>%
  st_drop_geometry() %>%
  left_join(
    areas %>%
    st_drop_geometry() %>%
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
            cluster_id = centroidid,
            cluster_id,
            res_type = factor(urban, 1:2, c("urban", "rural")),
            survey_region_id = hprovince_za) %>%
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

survey_region_areas <- survey_regions %>%
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

p3 <- p2 +
  geom_point(
    data = survey_clusters %>%
      full_join(select(st_drop_geometry(survey_regions), survey_region_id, survey_region_name)),
    aes(x = longitude, y = latitude, color = survey_region_name,
        size = as.numeric(geoloc_distance > 0))) +
  scale_size(range = c(1, 2))

ggsave("check/zamphia-survey-regions_clusters.pdf", p3, h = 5, w = 7)

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
  summarise(female_age_min = min(if_else(sex == "female", age, NA_integer_), na.rm=TRUE),
            female_age_max = max(if_else(sex == "female", age, NA_integer_), na.rm=TRUE),
            male_age_min = min(if_else(sex == "male", age, NA_integer_), na.rm=TRUE),
            male_age_max = max(if_else(sex == "male", age, NA_integer_), na.rm=TRUE),
            .groups = "drop") %>%
  mutate(iso3 = substr(survey_id, 1, 3),
         country = country,
         survey_type = "PHIA",
         survey_mid_calendar_quarter = recode(iso3, "UGA" = survey_mid_calendar_quarter),
         fieldwork_start = fieldwork_start,
         fieldwork_end   = fieldwork_end)

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

#' survey_indicators <- calc_survey_hiv_indicators(
#'                               survey_meta,
#'                               survey_regions,
#'                               survey_clusters,
#'                               survey_individuals,
#'                               survey_biomarker,
#'                               as.data.frame(areas)
#'                             )
#'
#'
#' #' ## Save survey indicators dataset
#' write_csv(survey_indicators, paste0(tolower(survey_id), "_hiv_indicators.csv"), na = "")


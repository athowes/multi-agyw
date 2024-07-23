
iso3 <- "BWA"
country <- "Botswana"
survey_id  <- "BWA2021BAIS"
survey_mid_calendar_quarter <- "CY2021Q2"


#' ## Load area hierarchy
areas <- read_sf("depends/bwa_areas.geojson")
areas <- st_make_valid(areas)


## #' ## Load PHIA datasets
## sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")

## phia_path <- "sites/HIVInferenceGroup-WP/Shared Documents/Data/household surveys/Botswana/BAIS V/datasets/"

## path <- c("baisv2021adultbio.dta.zip",
##           "baisv2021hh.dta.zip",
##           "baisv2021adultind.dta.zip",
##           "baisv2021child.dta.zip",
##           "baisv2021roster.dta.zip") %>%
##   file.path(phia_path, .) %>%
##   URLencode()

## ## NOTE: coordinate data set not yet available

## file <- sharepoint$download(path)

## geo <- rdhs::read_zipdata(phia_files$geo)
## hh <- rdhs::read_zipdata(file, "baisv2021hh.dta")
## bio <- rdhs::read_zipdata(file, "baisv2021adultbio.dta")
## ind <- rdhs::read_zipdata(file, "baisv2021adultind.dta")
## chbio <- rdhs::read_zipdata(file, "baisv2021childbio.dta")
## chind <- rdhs::read_zipdata(file, "baisv2021childind.dta")

# path <- "~/Data/household surveys/Botswana/BAIS V/datasets/"
#
# bio <- read_dta(file.path(path, "baisv2021adultbio.dta.zip"))
# hh <- read_dta(file.path(path, "baisv2021hh.dta.zip"))
# ind <- read_dta(file.path(path, "baisv2021adultind.dta.zip"))
# chind <- read_dta(file.path(path, "baisv2021child.dta.zip"))
# roster <- read_dta(file.path(path, "baisv2021roster.dta.zip"))

bio <- read_dta("baisv2021adultbio.dta")
hh <- read_dta("baisv2021hh.dta")
ind <- read_dta("baisv2021adultind.dta")
chind <- read_dta("baisv2021child.dta")
roster <- read_dta("baisv2021roster.dta")

#' Note: Religion and ethnic group WERE asked. But CD4 count not done
#'
phia <- ind %>%
  filter(indstatus == 1) %>%  # Respondent
  select(centroidid, hdistrict_bw, urban, householdid,
         personid, surveystyear, surveystmonth,
         intwt0, gender, age,
         mcstatus, mcage, mcwhomed, mcwhotrad, mcdetail) %>%
  full_join(
    bio %>%
      filter(bt_status == 1) %>%
      select(personid, btwt0, hivstatusfinal, arvstatus, artselfreported,
             cd4count, vls, recentlagvlarv),
    by = "personid"
  ) %>%
  mutate(survey_id = survey_id,
         cluster_id = centroidid)


chphia <- chind %>%
  filter(indstatus == 1) %>%
  select(centroidid, hdistrict_bw, urban, householdid,
         personid, surveystyear, surveystmonth,
         chwt_bw0, gender, age,
         hivstatusfinal, arvstatus, cd4count, vls) %>%
  mutate(survey_id = survey_id,
         cluster_id = centroidid)



#' ## Survey regions
#'
#' This table identifies the smallest area in the area hierarchy which contains each
#' region in the survey stratification, which is the smallest area to which a cluster
#' can be assigned with certainty.

#' **11 November 2023**
#' NOTE: Survey health district clusters do not align to clusters used for Naomi.
#' APPROXIMATE assignment for test data.
#'
#' * 80 Ghanzi (14 clusters): Gantsi has 9 clusters, Charleshill has 5 clusters
#' * 50 Serowe Palapye (15 clusters): Serowe has 7 clusters, Palapye has 7 clusters
#' * 10 Ngwaketse South (19 clusters): 5 clusters in Moshupa, 12 clusters in Kanye
#'   - Assign 1 each extra to both

hh$survey_region_id <- hh$hdistrict_bw # different in each survey dataset depending on survey stratification


survey_region_id <- c("Gaborone" = 1,
                      "Francistown" = 2,
                      "Lobatse" = 3,
                      "Selibe Phikwe" = 4,
                      "Orapa" = 5,
                      "Jwaneng" = 6,
                      "Sowa" = 7,
                      "Ngwaketse South" = 10,
                      "Borolong" = 11,
                      "Ngwaketse West" = 12,
                      "South East" = 20,
                      "Kweneng East" = 30,
                      "Kweneng West" = 31,
                      "Kgatleng" = 40,
                      "Serowe Palapye" = 50,
                      "Central Mahalapye" = 51,
                      "Central Bobonong" = 52,
                      "Central Boteti" = 53,
                      "Central Tutume" = 54,
                      "North East" = 60,
                      "Ngamiland East" = 70,
                      "Ngamiland West" = 71,
                      "Chobe" = 72,
                      "Ghanzi" = 80,
                      "Kgalagadi South" = 90,
                      "Kgalagadi North" = 91)

areas %>% filter(area_level == 3) %>% select(area_id, area_name)

survey_region_area_id <- c("Gaborone" =             "BWA_3_20it",
                           "Francistown" =          "BWA_3_16la",
                           "Lobatse" =              "BWA_3_22am",
                           "Selibe Phikwe" =        "BWA_3_03fy",
                           "Orapa" =                "BWA_3_01ci",
                           "Jwaneng" =              "BWA_3_24hp",
                           "Sowa" =                 "BWA_3_07cw",
                           "Ngwaketse South" =      "BWA_2_18so",
                           "Borolong" =             "BWA_3_23qq",
                           "Ngwaketse West" =       "BWA_3_25be",
                           "South East" =           "BWA_3_21zt",
                           "Kweneng East" =         "BWA_3_14bv",
                           "Kweneng West" =         "BWA_3_15ll",
                           "Kgatleng" =             "BWA_3_13bi",
                           "Serowe Palapye" =       "BWA_2_04is",
                           "Central Mahalapye" =    "BWA_3_04br",
                           "Central Bobonong" =     "BWA_3_02sw",
                           "Central Boteti" =       "BWA_3_01ci",
                           "Central Tutume" =       "BWA_3_07cw",
                           "North East" =           "BWA_3_17kp",
                           "Ngamiland East" =       "BWA_3_18zk",
                           "Ngamiland West" =       "BWA_3_19wt",
                           "Chobe" =                "BWA_3_08sk",
                           "Ghanzi" =               "BWA_2_07gc",
                           "Kgalagadi South" =      "BWA_3_12po",
                           "Kgalagadi North" =      "BWA_3_11ah")

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
  geom_sf(data = areas %>% filter(area_level == 1), fill = NA, inherit.aes = FALSE)

#' Inspect area_id assigments to confirm

survey_regions %>%
  left_join(
    areas %>%
      as.data.frame() %>%
      select(area_id, area_name, area_level, area_level_label),
    by = c("survey_region_area_id" = "area_id")
  ) %>%
  st_drop_geometry() %>%
  print(n = Inf)


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
  mutate(longitude = NA, latitude = NA) %>%
  ## left_join(geo, by = c("cluster_id" = "centroidid")) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), remove = FALSE, na.fail = FALSE) %>%
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

#'
#' TEMPORARY: Assign random distances
print("BOTSWANA TEMPORARY ASSIGNMENT")
set.seed(1)
survey_clusters$distance <- runif(nrow(survey_clusters))

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

mcstatus_labels <- c(`1` = "Yes, fully circumcised",
                     `2` = "Yes, partially circumcised",
                     `3` = "No",
                     `-8` = "Don't know",
                     `-9` = "Refused")

mcstatus_recode <- c(`1` = "Yes",
                     `2` = "Yes",
                     `3` = "No",
                     `-8` = NA_character_,
                     `-9` = NA_character_)

mcwho_labels = c(`1` = "Healthcare worker",
                 `2` = "Traditional practitioner / circumciser",
                 `96` = "Other",
                 `-8` = "Don't know",
                 `-9` = "Refused")

mcwho_recode = c(`1` = "Healthcare worker",
                 `2` = "Traditional practitioner",
                 `96` = "Traditional practitioner",
                 `-8` = NA_character_,
                 `-9` = NA_character_)

mcdetail_who_recode <- c(`1` = "Healthcare worker",
                         `2` = "Traditional practitioner")


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
      ),
    chphia %>%
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
        indweight = chwt_bw0
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
        recent = case_when(recentlagvlarv == 1 ~ 1,
                           recentlagvlarv == 2 ~ 0)
      )
   ,
    chphia %>%
      filter(!is.na(hivstatusfinal)) %>%
      transmute(
        survey_id,
        individual_id = personid,
        hivweight = chwt_bw0,
        hivstatus = case_when(hivstatusfinal == 1 ~ 1,
                              hivstatusfinal == 2 ~ 0),
        arv = case_when(arvstatus == 1 ~ 1,
                        arvstatus == 2 ~ 0),
        vls = case_when(vls == 1 ~ 1,
                        vls == 2 ~ 0),
        recent = NA_integer_
      )
  ) %>%
  mutate(hivweight = hivweight / mean(hivweight, na.rm = TRUE))


survey_circumcision <- phia %>%
  filter(gender == 1) %>%
  transmute(
    survey_id,
    individual_id = personid,
    circumcised = recode(as.integer(mcstatus),
                         `3` = 0L , `1` = 1L, `2` = 1L, .default = NA_integer_),
    circ_age = mcage,
    circ_where = NA_character_,
    circ_who = recode(as.integer(mcdetail), !!!mcdetail_who_recode, .default = NA_character_)
  )



survey_meta <- survey_individuals %>%
  group_by(survey_id) %>%
  summarise(female_age_min = min(if_else(sex == "female", age, NA_integer_), na.rm=TRUE),
            female_age_max = max(if_else(sex == "female", age, NA_integer_), na.rm=TRUE),
            male_age_min = min(if_else(sex == "male", age, NA_integer_), na.rm=TRUE),
            male_age_max = max(if_else(sex == "male", age, NA_integer_), na.rm=TRUE),
            .groups = "drop") %>%
  mutate(iso3 = substr(survey_id, 1, 3),
         country = country,
         survey_type = "BAIS",
         survey_mid_calendar_quarter = survey_mid_calendar_quarter,
         fieldwork_start = NA,
         fieldwork_end   = NA)

survey_sexbehav <- extract_sexbehav_phia(ind, survey_id)
(misallocation <- check_survey_sexbehav(survey_sexbehav))

#' ## Save survey datasets

write_csv(survey_meta, paste0(tolower(survey_id), "_survey_meta.csv"), na = "")
write_csv(survey_regions, paste0(tolower(survey_id), "_survey_regions.csv"), na = "")
write_csv(survey_clusters, paste0(tolower(survey_id), "_survey_clusters.csv"), na = "")
write_csv(survey_individuals, paste0(tolower(survey_id), "_survey_individuals.csv"), na = "")
write_csv(survey_biomarker, paste0(tolower(survey_id), "_survey_biomarker.csv"), na = "")
write_csv(survey_circumcision, paste0(tolower(survey_id), "_survey_circumcision.csv"), na = "")
write_csv(survey_sexbehav, paste0(tolower(survey_id), "_survey_sexbehav.csv"), na = "")


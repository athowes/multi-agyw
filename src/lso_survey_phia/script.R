#' ## Survey meta data

iso3 <- "LSO"
country <- "Lesotho"
survey_id  <- "LSO2017PHIA"
survey_mid_calendar_quarter <- "CY2017Q4"
fieldwork_start <- NA
fieldwork_end <- NA

#' ## Load area hierarchy
areas <- read_sf("depends/lso_areas.geojson")


#' ## Load PHIA datasets
sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")

phia_path <- "sites/HIVInferenceGroup-WP/Shared Documents/Data/household surveys/PHIA/datasets/LSO/datasets"

paths <- list(geo = "LePHIA 2016-2017 PR Geospatial Data 20210518.zip",
              hh = "102_LePHIA 2016-2017 Household Dataset (DTA).zip",
              ind = "202_LePHIA 2016-2017 Adult Interview Dataset (DTA).zip",
              bio = "302_LePHIA 2016-2017 Adult Biomarker Dataset (DTA).zip",
              chind = "205_LePHIA 2016-2017 Child Interview Dataset (DTA).zip",
              chbio = "305_LePHIA 2016-2017 Child Biomarker Dataset (DTA).zip") %>%
  lapply(function(x) file.path(phia_path, x)) %>%
  lapply(URLencode)

phia_files <- lapply(paths, sharepoint$download)

geo <- rdhs::read_zipdata(phia_files$geo)

hh <- rdhs::read_zipdata(phia_files$hh)
bio <- rdhs::read_zipdata(phia_files$bio)
ind <- rdhs::read_zipdata(phia_files$ind)
chbio <- rdhs::read_zipdata(phia_files$chbio)
chind <- rdhs::read_zipdata(phia_files$chind)

phia <- ind %>%
  filter(indstatus == 1) %>%  # Respondent
  select(centroidid, district, urban, householdid,
         personid, surveystyear, surveystmonth,
         intwt0, gender, age, religion,
         mcstatus, mcage, mcwho) %>%
  full_join(
    bio %>%
      filter(bt_status == 1) %>%
      select(personid, btwt0, hivstatusfinal, arvstatus, artselfreported,
             vls, cd4count, recentlagvlarv),
    by = "personid"
  ) %>%
  mutate(survey_id = survey_id,
         cluster_id = centroidid)


#' Note: Religion was not asked for children. Strategy to
#'       assign these based on `momid` if available, otherwise modal value
#'       from the household. But the primary motivation for these variables
#'       is for circumcision analysis, which is not asked for children.
#'

chphia <- chind %>%
  filter(indstatus == 1) %>%
  select(centroidid, district, urban, householdid,
         personid, surveystyear, surveystmonth,
         intwt0, gender, age, agem) %>%
  full_join(
    chbio %>%
      filter(bt_status == 1) %>%
      select(personid, btwt0, hivstatusfinal, arvstatus, pedartparentreported,
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

hh$survey_region_id <- hh$district # different in each survey dataset depending on survey stratification

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



#' ## Survey individuals dataset

religion_labels <- c(`1` = "Roman Catholic",
                     `2` = "Lesotho Evangelical",
                     `3` = "Anglican",
                     `4` = "Pentacostal",
                     `5` = "Other Christian",
                     `96` = "Other",
                     `-8` = "Don't know",
                     `-9` = "Refused")

mcstatus_lables <- c(`1` = "Yes",
                     `2` = "No",
                     `-8` = "Don't know",
                     `-9` = "Refused")

mcwho_labels = c(`1` = "Doctor, clinical officer, or nurse",
                 `2` = "Traditional practitioner / circumciser",
                 `3` = "Midwife",
                 `96` = "Other",
                 `-8` = "Don't know",
                 `-9` = "Refused")

mcwho_recode = c(`1` = "Healthcare worker",
                 `2` = "Traditional practitioner",
                 `3` = "Traditional practitioner",   ## TODO: UNSURE ON THIS
                 `96` = "Traditional practitioner",
                 `-8` = NA_character_,
                 `-9` = NA_character_)


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
        religion = recode(religion, !!!religion_labels),
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
        dob_cmc = interview_cmc - agem,
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
    ,
    chphia %>%
      filter(!is.na(hivstatusfinal)) %>%
      transmute(
        survey_id,
        individual_id = personid,
        hivweight = btwt0,
        hivstatus = case_when(hivstatusfinal == 1 ~ 1,
                              hivstatusfinal == 2 ~ 0),
        arv = case_when(arvstatus == 1 ~ 1,
                        arvstatus == 2 ~ 0),
        artself = case_when(pedartparentreported == 1 ~ 1,
                            pedartparentreported == 2 ~ 0),
        vls = case_when(vls == 1 ~ 1,
                        vls == 2 ~ 0),
        cd4 = cd4count,
        recent = case_when(recentlagvlarv == 1 ~ 1,
                           recentlagvlarv == 2 ~ 0)
      )
  ) %>%
  mutate(hivweight = hivweight / mean(hivweight, na.rm = TRUE))


survey_circumcision <- phia %>%
  filter(gender == 1) %>%
  transmute(
    survey_id,
    individual_id = personid,
    circumcised = recode(mcstatus, `2` = 0L , `1` = 1L, .default = NA_integer_),
    circ_age = mcage,
    circ_where = NA_character_,
    circ_who = recode(mcwho, !!!mcwho_recode)
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
         survey_type = "PHIA",
         survey_mid_calendar_quarter = recode(iso3, "MWI" = survey_mid_calendar_quarter),
         fieldwork_start = fieldwork_start,
         fieldwork_end   = fieldwork_end)

#' ## Save survey datasets

write_csv(survey_meta, paste0(tolower(survey_id), "_survey_meta.csv"), na = "")
write_csv(survey_regions, paste0(tolower(survey_id), "_survey_regions.csv"), na = "")
write_csv(survey_clusters, paste0(tolower(survey_id), "_survey_clusters.csv"), na = "")
write_csv(survey_individuals, paste0(tolower(survey_id), "_survey_individuals.csv"), na = "")
write_csv(survey_biomarker, paste0(tolower(survey_id), "_survey_biomarker.csv"), na = "")
write_csv(survey_circumcision, paste0(tolower(survey_id), "_survey_circumcision.csv"), na = "")

extract_sexbehav_phia <- function(ind) {
  #' All of the sexual behaviour variables we're interested in
  #' From LePHIA 2016 - 2017 Adult Questionaire
  sb_vars <- c(
    "firstsxage", #' Age at first vaginal sex
    "firstsxagedk", #' Age at first vaginal sex (don't know)
    "analsxever", #' Age at first anal sex
    "lifetimesex", #' Total sexual partners (lifetime)
    "lifetimesexdk", #' Total sexual partners (lifetime) (don't know)
    "part12monum", #' Total sexual partners (past 12 months)
    "part12modkr", #' Total sexual partners (past 12 months) (don't know)
    paste0("partlivew", 1:3), #' Does partner i live in this household
    paste0("partrelation", 1:3), #' Relationship to partner i
    paste0("partlastsup", 1:3), #' Expectation of gifts, payment, other help with partner i
    paste0("partlastsxtimed", 1:3), #' How long since last sex with partner i
    "sellsx12mo", #' Had sex for money, gifts during past 12 months
    "buysx12mo" #' Paid money or given gifts for sex during past 12 months
  )

  ind %>%
    mutate(
      survey_id = survey_id,
      individual_id = personid
    ) %>%
    select(survey_id, individual_id, all_of(sb_vars)) %>%
    mutate(
      #' Reports sexual activity in the last 12 months
      sex12m = case_when(
        (is.na(firstsxage) & (firstsxagedk == 96)) & (analsxever %in% c(2, -8, -9)) ~ FALSE, #' 96 is code for no sex
        part12monum > 0 ~ TRUE,
        part12modkr == -8 ~ TRUE, #' If don't know number of partners, assume > 1
        TRUE ~ FALSE
      ),
      #' Does not report sexual activity in the last 12 months
      nosex12m = case_when(
        sex12m == TRUE ~ FALSE,
        sex12m == FALSE ~ TRUE,
        is.na(sex12m) ~ NA
      ),
      #' Reports sexual activity with exactly one cohabiting partner in the past 12 months
      sexcohab = case_when(
        sex12m == FALSE ~ FALSE,
        (part12monum == 1) & (partlivew1 == 1) ~ TRUE,
        TRUE ~ FALSE
      ),
      #' Reports one or more non-regular sexual partner
      sexnonreg = case_when(
        nosex12m == TRUE ~ FALSE,
        part12monum > 1 ~ TRUE,
        (part12monum == 1) & (partlivew1 == 2) ~ TRUE,
        TRUE ~ FALSE
      ),
      #' Reports having exchanged gifts, cash, or anything else for sex in the past 12 months
      sexpaid12m = case_when(
        sellsx12mo == 1 ~ TRUE,
        buysx12mo == 1 ~ TRUE,
        TRUE ~ FALSE
      ),
      #' Either sexnonreg or sexpaid12m
      sexnonregplus = case_when(
        sexnonreg == TRUE ~ TRUE,
        sexpaid12m == TRUE ~ TRUE,
        TRUE ~ FALSE
      ),
      #' Just want the highest risk category that an individual belongs to
      nosex12m = ifelse(sexcohab | sexnonreg | sexpaid12m, FALSE, nosex12m),
      sexcohab = ifelse(sexnonreg | sexpaid12m, FALSE, sexcohab),
      sexnonreg = ifelse(sexpaid12m, FALSE, sexnonreg),
      #' Turn everything from TRUE / FALSE coding to 1 / 0
      across(sex12m:sexnonregplus, ~ as.numeric(.x))
    ) %>%
    select(-all_of(sb_vars))
}

survey_sexbehav <- extract_sexbehav_phia(ind)

check_survey_sexbehav <- function(survey_sexbehav) {
  df <- survey_sexbehav %>%
    mutate(
      r_tot = nosex12m + sexcohab + sexnonreg + sexpaid12m
    )

  cat(
    paste0(
      "The proportion of rows allocatated to one and only one category is ",
      round(sum(df$r_tot == 1) / nrow(df), 3) * 100, "%.\n",
      "The following rows are incorrectly allocated to multiple, or no, categories:\n"
    )
  )

  df %>%
    filter(r_tot != 1)
}

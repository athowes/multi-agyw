#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("bwa_survey_behav")
# setwd("src/bwa_survey_behav")

areas <- read_sf("depends/bwa_areas.geojson")
raw <- read_csv("depends/bwa2013bais-recode-sexbehav.csv")

areas_wide <- spread_areas(areas)
raw$survey_id <- "BWA2013BAIS"

survey_regions <- raw %>%
  select(survey_id,
         survey_region_id = district_code,
         survey_region_name = district_name) %>%
  distinct()

survey_clusters <- raw %>%
  transmute(survey_id,
            cluster_id,
            survey_region_id = district_code,
            res_type = urban_rural %>%
              recode("Cities" = "urban",
                     "Rural" = "rural",
                     "Towns" = "urban",
                     "Urban Villages" = "urban"),
            longitude,
            latitude) %>%
  distinct() %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), remove = FALSE) %>%
  sf::`st_crs<-`(4326) %>%
  as.data.frame()

#' Recode list assembled by plotting survey clusters vs. areas
survey_clusters %>%
  left_join(survey_regions, by = c("survey_id", "survey_region_id")) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(data = areas_wide) +
  geom_sf_text(aes(label = survey_region_id, color = survey_region_name),
               size = 1.5, fontface = "bold")

dev.off()

survey_areas_recode <- list(
  `1` = "BWA_3_20it",
  `2` = "BWA_3_16la",
  `3` = "BWA_3_22am",
  `4` = "BWA_3_03fy",
  `5` = "BWA_3_01ci",
  `6` = "BWA_3_24hp",
  `7` = "BWA_3_07cw",
  `10` = c("BWA_3_26he", "BWA_3_27dj"),
  `11` = "BWA_3_23qq",
  `12` = "BWA_3_25be",
  `20` = "BWA_3_21zt",
  `30` = "BWA_3_14bv",
  `31` = "BWA_3_15ll",
  `40` = "BWA_3_13bi",
  `50` = c("BWA_3_05qx", "BWA_3_06kn"),
  `51` = "BWA_3_04br",
  `52` = "BWA_3_02sw",
  `53` = "BWA_3_01ci",
  `54` = "BWA_3_07cw",
  `60` = "BWA_3_17kp",
  `70` = "BWA_3_18zk",
  `71` = "BWA_3_19wt",
  `72` = "BWA_3_08sk",
  `80` = c("BWA_3_09ro", "BWA_3_10ox"),
  `90` = "BWA_3_12po",
  `91` = "BWA_3_11ah"
)

stopifnot(unlist(survey_areas_recode) %in% areas$area_id)

survey_region_areas <- survey_regions %>%
  full_join(
    Map(data.frame,
        survey_region_id = names(survey_areas_recode),
        area_id = survey_areas_recode) %>%
      bind_rows() %>%
      mutate(survey_region_id = as.integer(survey_region_id)),
    by = "survey_region_id"
  ) %>%
  full_join(
    select(areas_wide, starts_with("area_id")),
    by = "area_id"
  ) %>%
  st_as_sf()

survey_regions <- create_survey_regions_dhs(survey_region_areas)

survey_clusters <- assign_dhs_cluster_areas(survey_clusters, survey_region_areas)

survey_region_boundaries <- survey_regions %>%
  mutate(
    survey_region_area_id = case_when(
      survey_region_name == "Kweneng East" ~ "BWA_3_14to",
      TRUE ~ survey_region_area_id
    )
  ) %>%
  left_join(areas, by = c("survey_region_area_id" = "area_id")) %>%
  st_as_sf()


p_coord_check <- plot_survey_coordinate_check(
  survey_clusters,
  survey_region_boundaries,
  survey_region_areas
)

dir.create("check")
pdf("check/bwa2013bais-cluster-check.pdf", h = 5, w = 7)
p_coord_check
dev.off()

#' Survey individuals dataset

#' Create individuals data
survey_individuals <- raw %>%
  group_by(survey_id) %>%  # for normalizing weights
  transmute(
    cluster_id,
    individual_id,
    household = NA,
    line = NA,
    interview_cmc = NA,
    sex,
    age = as.integer(age),
    dob_cmc = NA,
    indweight = Weight1 / mean(Weight1)
  )

survey_biomarker <- raw %>%
  group_by(survey_id) %>%  #' For normalizing weights
  transmute(
    individual_id,
    hivweight = Weight1 / mean(Weight1),
    hivstatus = hivstatus,
    arv = NA,
    artself,
    vls = NA,
    cd4 = NA,
    recent = NA
  ) %>%
  ungroup()

#' Survey meta data
survey_meta <- survey_individuals %>%
  group_by(survey_id) %>%
  summarise(
    female_age_min = min(if_else(sex == "female", age, NA_integer_), na.rm = TRUE),
    female_age_max = max(if_else(sex == "female", age, NA_integer_), na.rm = TRUE),
    male_age_min = min(if_else(sex == "male", age, NA_integer_), na.rm = TRUE),
    male_age_max = max(if_else(sex == "male", age, NA_integer_), na.rm = TRUE)
  ) %>%
  mutate(survey_mid_calendar_quarter = "CY2013Q1") %>%
  ungroup()

#' Create sexual behaviour data
survey_sexbehav <- raw %>%
  group_by(survey_id) %>%  # for normalizing weights
  transmute(
    individual_id,
    sex12m,
    nosex12m,
    sexcohab,
    sexcohabspouse,
    sexnonreg,
    sexnonregspouse,
    sexpaid12m,
    sexnonregplus,
    sexnonregspouseplus,
    giftsvar
  )
(misallocation <- check_survey_sexbehav(survey_sexbehav))

survey_other <- list(survey_sexbehav)

age_group_include <- c("Y015_019", "Y020_024", "Y025_029", "Y030_034", "Y035_039",
                       "Y040_044", "Y045_049", "Y015_024", "Y025_049", "Y015_049")
sex <- c("female")

#' Survey indicator dataset
survey_indicators <- calc_survey_indicators(
  survey_meta,
  survey_regions,
  survey_clusters,
  survey_individuals,
  survey_biomarker,
  survey_other,
  as.data.frame(areas),
  age_group_include = age_group_include,
  sex = sex,
  area_bottom_level = 3
)

#' Save survey indicators dataset
write_csv(survey_indicators, "bwa_survey_indicators_sexbehav.csv", na = "")

#' Save formatted datasets
write_csv(survey_meta, "bwa2013bais_survey_meta.csv", na = "")
write_csv(survey_regions, "bwa2013bais_survey_regions.csv", na = "")
write_csv(survey_clusters, "bwa2013bais_survey_clusters.csv", na = "")
write_csv(survey_individuals, "bwa2013bais_survey_individuals.csv", na = "")
write_csv(survey_biomarker, "bwa2013bais_survey_biomarker.csv", na = "")
write_csv(survey_sexbehav, "bwa2013bais_survey_sexbehav.csv", na = "")

#' Get prevalence estimates for different sexual behaviours
survey_sexbehav_reduced <- survey_sexbehav %>%
  select(-sex12m, -sexcohabspouse, -sexnonregspouse, -giftsvar, -sexnonregplus, -sexnonregspouseplus)

hiv_indicators <- calc_survey_hiv_indicators(
  survey_meta,
  survey_regions,
  survey_clusters,
  survey_individuals,
  survey_biomarker,
  survey_other = list(survey_sexbehav_reduced),
  st_drop_geometry(areas),
  sex = sex,
  age_group_include = age_group_include,
  area_top_level = 0,
  area_bottom_level = 0,
  formula = ~ indicator + survey_id + area_id + res_type + sex + age_group +
    nosex12m + sexcohab + sexnonreg + sexpaid12m
)

#' Keep only the stratifications with "all" in everything but the indicator itself
hiv_indicators <- hiv_indicators %>%
  filter(
    rowSums(across(.cols = nosex12m:sexpaid12m, ~ .x == "all")) %in% c(3, 4) &
    rowSums(across(.cols = nosex12m:sexpaid12m, ~ is.na(.x))) == 0
  )

#' Save HIV indicators dataset
write_csv(hiv_indicators, "bwa_hiv_indicators_sexbehav.csv", na = "")

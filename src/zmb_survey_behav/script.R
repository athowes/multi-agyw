#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("zmb_survey_behav")
# setwd("src/zmb_survey_behav")

#' ISO3 country code
iso3 <- "ZMB"

#' Load area hierarchy
areas <- read_sf("depends/zmb_areas.geojson")
areas_wide <- naomi::spread_areas(areas)

#' Even though STATcomplier thinks that the Zambia 2001-02 DHS has relevant questions, we do not
#' include it  as there is no released GPS dataset, preventing clusters being snapped to districts
#' https://dhsprogram.com/data/available-datasets.cfm
surveys <- create_surveys_dhs(iso3, survey_characteristics = NULL) %>%
  filter(
    SurveyId %in% c("ZM2007DHS", "ZM2013DHS", "ZM2018DHS")
  )

survey_meta <- create_survey_meta_dhs(surveys)

#' These should be integers, not characters
survey_meta$male_age_min <- as.integer(survey_meta$male_age_min)
survey_meta$male_age_max <- as.integer(survey_meta$male_age_max)

survey_region_boundaries <- create_survey_boundaries_dhs(surveys)
surveys <- surveys_add_dhs_regvar(surveys, survey_region_boundaries)

#' REGVAR miscoded as "v024" instead of "hv024" in ZM2013DHS
surveys$REGVAR[surveys$survey_id == "ZMB2013DHS"] <- "hv024"

survey_region_boundaries <- st_make_valid(survey_region_boundaries)

#' Allocate each area to survey region
survey_region_areas <- allocate_areas_survey_regions(areas_wide, survey_region_boundaries)
validate_survey_region_areas(survey_region_areas, survey_region_boundaries)

survey_regions <- create_survey_regions_dhs(survey_region_areas)

#' Survey clusters dataset
survey_clusters <- create_survey_clusters_dhs(surveys)

#' Recode (0, 0) survey cluster in ZM2018DHS to NA
filter(survey_clusters, abs(longitude) < 0.001)

survey_clusters  <- survey_clusters %>%
  mutate(
    longitude = if_else(abs(longitude) < 0.0001 & abs(latitude) < 0.0001, NA_real_, longitude),
    latitude = if_else(abs(latitude) < 0.0001 & abs(latitude) < 0.0001, NA_real_, latitude)
  )

filter(survey_clusters, abs(longitude) < 0.001)

#' Snap survey clusters to areas
survey_clusters <- assign_dhs_cluster_areas(survey_clusters, survey_region_areas)

p_coord_check <- plot_survey_coordinate_check(
  survey_clusters,
  survey_region_boundaries,
  survey_region_areas
)

dir.create("check")
pdf(paste0("check/", tolower(iso3), "_dhs-cluster-check.pdf"), h = 5, w = 7)
p_coord_check
dev.off()

#' Individual dataset
individuals <- create_individual_hiv_dhs(surveys)
names(individuals)

survey_individuals <- create_survey_individuals_dhs(individuals)
names(survey_individuals)

survey_biomarker <- create_survey_biomarker_dhs(individuals)
names(survey_biomarker)

#' Extract sexual behaviour characteristics from the survey
survey_sexbehav <- create_sexbehav_dhs(surveys)
names(survey_sexbehav)
(misallocation <- check_survey_sexbehav(survey_sexbehav))

survey_other <- list(survey_sexbehav)

age_group_include <- c("Y015_019", "Y020_024", "Y025_029", "Y015_024")
sex <- c("female")

#' Survey indicator dataset
survey_indicators <- calc_survey_indicators(
  survey_meta,
  survey_regions,
  survey_clusters,
  survey_individuals,
  survey_biomarker,
  survey_other,
  st_drop_geometry(areas),
  sex = sex,
  age_group_include = age_group_include
)

#' PHIA data
phia_survey_meta <- read_csv("depends/zmb2016phia_survey_meta.csv")
phia_survey_regions <- read_csv("depends/zmb2016phia_survey_regions.csv")
phia_survey_clusters <- read_csv("depends/zmb2016phia_survey_clusters.csv")
phia_survey_individuals <- read_csv("depends/zmb2016phia_survey_individuals.csv")
phia_survey_biomarker <- read_csv("depends/zmb2016phia_survey_biomarker.csv")
phia_survey_sexbehav <- read_csv("depends/zmb2016phia_survey_sexbehav.csv")
(phia_misallocation <- check_survey_sexbehav(phia_survey_sexbehav))

#' PHIA survey indicator dataset
phia_survey_indicators <- calc_survey_indicators(
  phia_survey_meta,
  phia_survey_regions,
  phia_survey_clusters,
  phia_survey_individuals,
  phia_survey_biomarker,
  list(phia_survey_sexbehav),
  st_drop_geometry(areas),
  sex = sex,
  age_group_include = age_group_include
)

#' Combine all surveys together
survey_indicators <- bind_rows(survey_indicators, phia_survey_indicators)

#' Save survey indicators dataset
write_csv(survey_indicators, "zmb_survey_indicators_sexbehav.csv", na = "")

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

phia_survey_sexbehav_reduced <- phia_survey_sexbehav %>%
  select(-sex12m, -sexcohabspouse, -sexnonregspouse, -giftsvar, -sexnonregplus, -sexnonregspouseplus)

phia_hiv_indicators <- calc_survey_hiv_indicators(
  phia_survey_meta,
  phia_survey_regions,
  phia_survey_clusters,
  phia_survey_individuals,
  phia_survey_biomarker,
  survey_other = list(phia_survey_sexbehav_reduced),
  st_drop_geometry(areas),
  sex = sex,
  age_group_include = age_group_include,
  area_top_level = 0,
  area_bottom_level = 0,
  formula = ~ indicator + survey_id + area_id + res_type + sex + age_group +
    nosex12m + sexcohab + sexnonreg + sexpaid12m
)

hiv_indicators <- bind_rows(hiv_indicators, phia_hiv_indicators)

#' Keep only the stratifications with "all" in everything but the indicator itself
hiv_indicators <- hiv_indicators %>%
  filter(
    rowSums(across(.cols = nosex12m:sexpaid12m, ~ .x == "all")) == c(3, 4) &
      rowSums(across(.cols = nosex12m:sexpaid12m, ~ is.na(.x))) == 0
  )

#' Save HIV indicators dataset
write_csv(hiv_indicators, "zmb_hiv_indicators_sexbehav.csv", na = "")

#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("tza_survey_behav")
# setwd("src/tza_survey_behav")

#' ISO3 country code
iso3 <- "TZA"

#' Load area hierarchy
areas <- read_sf("depends/tza_areas.geojson")
areas_wide <- naomi::spread_areas(areas)

surveys <- create_surveys_dhs(iso3, survey_characteristics = 24) %>%
  filter(as.numeric(SurveyYear) > 1998)

survey_meta <- create_survey_meta_dhs(surveys)

survey_region_boundaries <- create_survey_boundaries_dhs(surveys)

#' Boundary file for 2007 AIS has single region for Pemba and single region for Unguja, both coded 9999
#' But hv024 in the HR dataset for 2007 AIS has five regions (survey_region_id = 51:55); these are the
#' same as the 2010 DHS.
#' ACTION: Replace regions coded 9999 from the 2007 AIS with regions 51:55 from the 2010 DHS.

hrd07 <- dhs_datasets(surveyIds = "TZ2007AIS", fileType = "HR", fileFormat = "FL")
tz2007hr <- readRDS(get_datasets(hrd07)[[1]])

count(tz2007hr, hv024)

survey_region_boundaries <- survey_region_boundaries %>%
  filter( !(survey_id == "TZA2007AIS" & survey_region_id == 9999) ) %>%
  bind_rows(
    survey_region_boundaries %>%
    filter(survey_id == "TZA2010DHS",  survey_region_id %in% 51:55) %>%
    mutate(survey_id = "TZA2007AIS")
  )

surveys <- surveys_add_dhs_regvar(surveys, survey_region_boundaries)

#' Allocate each area to survey region
survey_region_areas <- allocate_areas_survey_regions(areas_wide, survey_region_boundaries)
validate_survey_region_areas(survey_region_areas, survey_region_boundaries)

survey_regions <- create_survey_regions_dhs(survey_region_areas)

#' Survey clusters dataset
survey_clusters <- create_survey_clusters_dhs(surveys)

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

#' Extract the individual characteristics from the survey
survey_individuals <- create_survey_individuals_dhs(individuals)
names(survey_individuals)

#' Extract the HIV related characteristics from the survey
survey_biomarker <- create_survey_biomarker_dhs(individuals)
names(survey_biomarker)

#' Extract sexual behaviour characteristics from the survey
survey_sexbehav <- create_sexbehav_dhs(surveys)
names(survey_sexbehav)

survey_other <- list(survey_sexbehav)

age_group_include <- c("Y015_019", "Y020_024", "Y025_029", "Y015_024")
sex <- c("female")

#' # Survey indicator dataset

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
phia_survey_meta <- read_csv("depends/tza2016phia_survey_meta.csv")
phia_survey_regions <- read_csv("depends/tza2016phia_survey_regions.csv")
phia_survey_clusters <- read_csv("depends/tza2016phia_survey_clusters.csv")
phia_survey_individuals <- read_csv("depends/tza2016phia_survey_individuals.csv")
phia_survey_biomarker <- read_csv("depends/tza2016phia_survey_biomarker.csv")
phia_survey_sexbehav <- read_csv("depends/tza2016phia_survey_sexbehav.csv")

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
write_csv(survey_indicators, "tza_survey_indicators_sexbehav.csv", na = "")

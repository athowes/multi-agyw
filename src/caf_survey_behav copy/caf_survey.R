# orderly::orderly_develop_start("caf_survey_mics")
# setwd("src/caf_survey_mics/")

#' ## Survey meta data

iso3 <- "CAF"
country <- "Central African Republic"
survey_id  <- "CAF2018MICS"
survey_mid_calendar_quarter <- "CY2018Q4"

#' ## Load area hierarchy
areas <- read_sf("depends/caf_areas.geojson")

age_group_include <- c("Y015_019", "Y020_024", "Y025_029", "Y030_034", "Y035_039",
                       "Y040_044", "Y045_049", "Y015_024", "Y025_049", "Y015_049")
sex <- c("male")

#' MICS data
mics_survey_meta <- read_csv("depends/caf2018mics_survey_meta.csv")
mics_survey_regions <- read_csv("depends/caf2018mics_survey_regions.csv")
mics_survey_clusters <- read_csv("depends/caf2018mics_survey_clusters.csv")
mics_survey_individuals <- read_csv("depends/caf2018mics_survey_individuals.csv")
mics_survey_sexbehav <- read_csv("depends/caf2018mics_survey_sexbehav.csv")
(mics_misallocation <- check_survey_sexbehav(mics_survey_sexbehav))

# placeholder biomarker data
mics_survey_biomarker <- data.frame(survey_id = survey_id,
                                    individual_id = mics_survey_individuals$individual_id,
                                    hivweight = NA,
                                    hivstatus = NA,
                                    arv = NA,
                                    artself = NA,
                                    vls = NA,
                                    cd4 = NA,
                                    recent = NA)

#' MICS survey indicator dataset
survey_indicators <- calc_survey_indicators(
  mics_survey_meta,
  mics_survey_regions,
  mics_survey_clusters,
  mics_survey_individuals,
  mics_survey_biomarker,
  list(mics_survey_sexbehav),
  st_drop_geometry(areas),
  sex = sex,
  age_group_include = age_group_include,
  area_bottom_level = 3
)

#' Save survey indicators dataset
write_csv(survey_indicators, "caf_survey_indicators_sexbehav.csv", na = "")

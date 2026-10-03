#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("nga_survey_behav")
# setwd("src/nga_survey_behav")

#' ISO3 country code
iso3 <- "NGA"

orderly_dependency("process_areas",
                   "latest()",
                   "nga_areas.geojson")
areas <- read_sf("nga_areas.geojson") %>% st_make_valid()
areas_wide <- naomi::spread_areas(areas)

surveys <- create_surveys_dhs(iso3, survey_characteristics = 24) %>%
  filter(as.numeric(SurveyYear) > 1998)

survey_meta <- create_survey_meta_dhs(surveys)

survey_region_boundaries <- create_survey_boundaries_dhs(surveys)
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
individuals <- create_individual_dhs(surveys)
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
(misallocation <- check_survey_sexbehav(survey_sexbehav))

survey_other <- list(survey_sexbehav)

age_group_include <- c("Y015_019", "Y020_024", "Y025_029", "Y030_034", "Y035_039",
                       "Y040_044", "Y045_049", "Y015_024", "Y025_049", "Y015_049")
sex <- c("male")

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

# MICS 2016
orderly_dependency("nga_survey_mics2016_men",
                   "latest()",
                   c("nga2016mics_survey_meta.csv",
                     "nga2016mics_survey_regions.csv",
                     "nga2016mics_survey_clusters.csv",
                     "nga2016mics_survey_individuals.csv",
                     "nga2016mics_survey_sexbehav.csv"))
mics_survey_meta_2016 <- read_csv("nga2016mics_survey_meta.csv")
mics_survey_regions_2016 <- read_csv("nga2016mics_survey_regions.csv")
mics_survey_clusters_2016 <- read_csv("nga2016mics_survey_clusters.csv")
mics_survey_individuals_2016 <- read_csv("nga2016mics_survey_individuals.csv")
mics_survey_sexbehav_2016 <- read_csv("nga2016mics_survey_sexbehav.csv")
(mics_misallocation_2016 <- check_survey_sexbehav(mics_survey_sexbehav_2016))

# placeholder biomarker data
mics_survey_biomarker <- data.frame(survey_id = "NGA2016MICS",
                                    individual_id = mics_survey_individuals_2016$individual_id,
                                    hivweight = NA,
                                    hivstatus = NA,
                                    arv = NA,
                                    artself = NA,
                                    vls = NA,
                                    cd4 = NA,
                                    recent = NA)

#' MICS survey indicator dataset 2016
mics_survey_indicators_2016 <- calc_survey_indicators(
  mics_survey_meta_2016,
  mics_survey_regions_2016,
  mics_survey_clusters_2016,
  mics_survey_individuals_2016,
  mics_survey_biomarker,
  list(mics_survey_sexbehav_2016),
  st_drop_geometry(areas),
  sex = sex,
  age_group_include = age_group_include,
)

# MICS 2021
orderly_dependency("nga_survey_mics2021_men",
                   "latest()",
                   c("nga2021mics_survey_meta.csv",
                     "nga2021mics_survey_regions.csv",
                     "nga2021mics_survey_clusters.csv",
                     "nga2021mics_survey_individuals.csv",
                     "nga2021mics_survey_sexbehav.csv"))
mics_survey_meta_2021 <- read_csv("nga2021mics_survey_meta.csv")
mics_survey_regions_2021 <- read_csv("nga2021mics_survey_regions.csv")
mics_survey_clusters_2021 <- read_csv("nga2021mics_survey_clusters.csv")
mics_survey_individuals_2021 <- read_csv("nga2021mics_survey_individuals.csv")
mics_survey_sexbehav_2021 <- read_csv("nga2021mics_survey_sexbehav.csv")
(mics_misallocation_2021 <- check_survey_sexbehav(mics_survey_sexbehav_2021))

# placeholder biomarker data
mics_survey_biomarker <- data.frame(survey_id = "NGA2021MICS",
                                    individual_id = mics_survey_individuals_2021$individual_id,
                                    hivweight = NA,
                                    hivstatus = NA,
                                    arv = NA,
                                    artself = NA,
                                    vls = NA,
                                    cd4 = NA,
                                    recent = NA)

#' MICS survey indicator dataset 2021
mics_survey_indicators_2021 <- calc_survey_indicators(
  mics_survey_meta_2021,
  mics_survey_regions_2021,
  mics_survey_clusters_2021,
  mics_survey_individuals_2021,
  mics_survey_biomarker,
  list(mics_survey_sexbehav_2021),
  st_drop_geometry(areas),
  sex = sex,
  age_group_include = age_group_include,
)

#' Combine all surveys together
survey_indicators <- bind_rows(survey_indicators, mics_survey_indicators_2016, mics_survey_indicators_2021)

#' Save survey indicator dataset
write_csv(survey_indicators, "nga_survey_indicators_sexbehav.csv", na = "")

#' #' Get prevalence estimates for different sexual behaviours
#' survey_sexbehav_reduced <- survey_sexbehav %>%
#'   select(-sex12m, -sexcohabspouse, -sexnonregspouse, -giftsvar, -sexnonregplus, -sexnonregspouseplus)
#'
#' hiv_indicators <- calc_survey_hiv_indicators(
#'   survey_meta,
#'   survey_regions,
#'   survey_clusters,
#'   survey_individuals,
#'   survey_biomarker,
#'   survey_other = list(survey_sexbehav_reduced),
#'   st_drop_geometry(areas),
#'   sex = sex,
#'   age_group_include = age_group_include,
#'   area_top_level = 0,
#'   area_bottom_level = 0,
#'   formula = ~ indicator + survey_id + area_id + res_type + sex + age_group +
#'     nosex12m + sexcohab + sexnonreg + sexpaid12m
#' )
#'
#' #' Keep only the stratifications with "all" in everything but the indicator itself
#' hiv_indicators <- hiv_indicators %>%
#'   filter(
#'     rowSums(across(.cols = nosex12m:sexpaid12m, ~ .x == "all")) %in% c(3, 4) &
#'       rowSums(across(.cols = nosex12m:sexpaid12m, ~ is.na(.x))) == 0
#'   )
#'
#' #' Save HIV indicators dataset
#' write_csv(hiv_indicators, "nga_hiv_indicators_sexbehav.csv", na = "")

#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("mli_survey_behav_men")
# setwd("src/mli_survey_behav_men")

#' ISO3 country code
iso3 <- "MLI"

areas <- read_sf("depends/mli_areas.geojson") %>%
  filter(area_level %in% 0:2)

areas_wide <- naomi::spread_areas(areas)

surveys <- create_surveys_dhs(iso3, survey_characteristics = 24) %>%
  filter(as.numeric(SurveyYear) > 1998 & as.numeric(SurveyYear)<2012)

survey_meta <- create_survey_meta_dhs(surveys)

survey_region_boundaries <- create_survey_boundaries_dhs(surveys)
surveys <- surveys_add_dhs_regvar(surveys, survey_region_boundaries)

survey_region_boundaries %>%
  filter(survey_id == "MLI2001DHS") %>%
  arrange(survey_region_id) %>%
  print(n = Inf)

#' For the MLI2001DHS, Kidal/Gao/Timbouctou are combined into one region in the
#' geographic datasets.Boundaries from areas files are nested within combined
#' region. Solution = remove combined geography and add in distinct geography

distinct_areas <- areas %>%
  filter(area_level == 1 & area_name %in% c("Tombouctou", "Kidal", "Gao"))

distinct_areas <- distinct_areas %>%
  select(survey_region_name = area_name, geometry) %>%
  mutate(survey_id = "MLI2001DHS", REGVAR = "hv024",
         survey_region_name = tolower(survey_region_name),
         survey_region_id = case_when(
           survey_region_name == "tombouctou" ~ 6,
           survey_region_name == "gao" ~ 7,
           survey_region_name == "kidal" ~ 8
         ))

#' Check that survey region IDs match with household recode
survey_region_boundaries <- rbind(survey_region_boundaries, distinct_areas) %>%
  filter(!(survey_id=="MLI2001DHS" & survey_region_id == 9999)) %>%
  arrange(survey_id, survey_region_id)

hr01 <- readRDS(get_datasets("mlhr41fl.ZIP")[[1]])
count(hr01, hv024, as_factor(hv024))

hr02 <- readRDS(get_datasets("mlhr6afl.ZIP")[[1]])
count(hr02, hv024, as_factor(hv024))

survey_region_boundaries %>%
  filter(survey_id == "MLI2001DHS") %>%
  arrange(survey_region_id) %>%
  print(n = Inf)

areas %>%
  filter(area_level == 1) %>%
  arrange(parent_area_id) %>%
  print(n = Inf)

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

#' Save survey indicator dataset
write_csv(survey_indicators, "mli_survey_indicators_sexbehav.csv", na = "")

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
write_csv(hiv_indicators, "mli_hiv_indicators_sexbehav.csv", na = "")

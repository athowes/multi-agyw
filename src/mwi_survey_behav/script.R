#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("mwi_survey_behav")
# setwd("src/mwi_survey_behav")

sf_use_s2(FALSE)

#' ISO3 country code
iso3 <- "MWI"

#' The spread_areas function "spreads area hierarchy to wide format"
areas <- read_sf("depends/mwi_areas.geojson")
areas_wide <- spread_areas(areas)

surveys <- create_surveys_dhs(iso3, survey_characteristics = 24) %>%
  filter(as.numeric(SurveyYear) > 1998)

survey_meta <- create_survey_meta_dhs(surveys)

#' "Create survey region boundaries dataset from DHS spatial data repository"
survey_region_boundaries <- create_survey_boundaries_dhs(surveys)

#' "Add REGVAR to surveys dataset"
#' REGVAR are regional variables in the household dataset. Here it's "shdist"
surveys <- surveys_add_dhs_regvar(surveys, survey_region_boundaries)

#' MW2015DHS HR dataset has four city REGCODES that do not appear in boundaries:
#' * 107: mzuzu city     [MWI_5_08]
#' * 210: lilongwe city  [MWI_5_18]
#' * 314: zomba city     [MWI_5_25]
#' * 315: blantyre city  [MWI_5_33]
#'
#' Add these to survey_region_boundaries and update district boundaries to exclude cities
#' From https://dhsprogram.com/data/File-Types-and-Names.cfm:
#' * HR stands for "Household Recode"
#' * FL stands for "Flat (ASCII) files"
hrd <- dhs_datasets(surveyIds = "MW2015DHS", fileType = "HR", fileFormat = "FL")
hr <- readRDS(get_datasets(hrd)[[1]]) #' There is only one survey here

hr15dhs_regions <- hr %>%
  distinct(
    survey_id = "MWI2015DHS",
    survey_region_id = shdist,
    survey_region_name = as_factor(shdist)
  )

#' All of the rows of hr15dhs_regions without a match in survey_region_boundaries (107, 210, 314, 315)
anti_join(
  hr15dhs_regions,
  survey_region_boundaries,
  by = c("survey_id", "survey_region_id")
)

metro_boundaries <- areas %>%
  filter(
    area_level == 5,
    grepl("City$", area_name)
  ) %>%
  select(area_id, area_name)

survey_region_id_recode <- c(
  "MWI_5_08" = 107,
  "MWI_5_18" = 210,
  "MWI_5_25" = 314,
  "MWI_5_33" = 315
)

survey_region_name_recode <- c(
  "MWI_5_08" = "Mzuzu City",
  "MWI_5_18" = "Lilongwe City",
  "MWI_5_25" = "Zomba City",
  "MWI_5_33" = "Blantyre City"
)

metro_boundaries <- metro_boundaries %>%
  mutate(
    survey_region_id = recode(area_id, !!!survey_region_id_recode),
    survey_region_name = recode(area_id, !!!survey_region_name_recode),
    survey_id = "MWI2015DHS",
    REGVAR = "shdistrict"
  ) %>%
  select(names(survey_region_boundaries))

regspl <- split(survey_region_boundaries, survey_region_boundaries$survey_id)

#' Remove metro boundaries from 28 districts
regspl$MWI2015DHS <- regspl$MWI2015DHS %>%
  st_difference(
    st_union(metro_boundaries)
  )

#' Append metros to MWI2015DHS survey regions
regspl$MWI2015DHS <- bind_rows(regspl$MWI2015DHS, metro_boundaries)

survey_region_boundaries <- bind_rows(regspl)

#' Check that all HR regions are in survey_region_boundaries
stopifnot(
  nrow(
    anti_join(hr15dhs_regions, survey_region_boundaries,
              by = c("survey_id", "survey_region_id"))
  ) == 0
)

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
pdf(paste0("check/", tolower(iso3), "_dhs-cluster-check.pdf"), h = 7, w = 6)
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
  age_group_include = age_group_include,
  area_bottom_level = 5
)

#' PHIA data
phia_survey_meta <- read_csv("depends/mwi2016phia_survey_meta.csv")
phia_survey_regions <- read_csv("depends/mwi2016phia_survey_regions.csv")
phia_survey_clusters <- read_csv("depends/mwi2016phia_survey_clusters.csv")
phia_survey_individuals <- read_csv("depends/mwi2016phia_survey_individuals.csv")
phia_survey_biomarker <- read_csv("depends/mwi2016phia_survey_biomarker.csv")
phia_survey_sexbehav <- read_csv("depends/mwi2016phia_survey_sexbehav.csv")
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
  age_group_include = age_group_include,
  area_bottom_level = 5
)

#' Combine all surveys together
survey_indicators <- bind_rows(survey_indicators, phia_survey_indicators)

#' Save survey indicators dataset
write_csv(survey_indicators, "mwi_survey_indicators_sexbehav.csv", na = "")

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
    rowSums(across(.cols = nosex12m:sexpaid12m, ~ .x == "all")) %in% c(3, 4) &
      rowSums(across(.cols = nosex12m:sexpaid12m, ~ is.na(.x))) == 0
  )

#' Save HIV indicators dataset
write_csv(hiv_indicators, "mwi_hiv_indicators_sexbehav.csv", na = "")

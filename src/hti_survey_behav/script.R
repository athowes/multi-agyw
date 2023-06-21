#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("hti_survey_behav")
# setwd("src/hti_survey_behav")

#' ISO3 country code
iso3 <- "HTI"

areas <- read_sf("depends/hti_areas.geojson") %>% st_make_valid()
areas_wide <- naomi::spread_areas(areas)

surveys <- create_surveys_dhs(iso3, survey_characteristics = 24) %>%
  filter(as.numeric(SurveyYear) > 1998)

survey_meta <- create_survey_meta_dhs(surveys)

survey_region_boundaries <- create_survey_boundaries_dhs(surveys)
surveys <- surveys_add_dhs_regvar(surveys, survey_region_boundaries)

#' For HTI2006 DHS: Aire Metropolitaine + Reste Ouest are included as separate
#' areas (ID = 9999) but are combined in HR recode file (ID = 1)
#' Separate areas from survey spatial data are nested within boundaries file
#' Solution:  Add in combined Ouest area from boundaries file to survey boundaries
#' and removed separate areas

#' For HTI2012 DHS:
#' (1) Same issue with Aire Metropolitaine + Reste Ouest in spatial data vs.
#'     combined area in HR recode
#' (2) Additional survey cluster in HR recode for camps surveyed (in Nippes region)
#' Solution: Add in combined area as done for HTI2006 and exclude camp clusters
#' (cluster ID == 11)

combined_ouest <- areas %>%
  filter(area_level == 1 & area_name == "Ouest")

ouest_2006 <- combined_ouest %>%
  select(survey_region_name = area_name, geometry) %>%
  mutate(survey_id = "HTI2006DHS", REGVAR = "hv024", survey_region_id = 1,
         survey_region_name = tolower(survey_region_name))

ouest_2012 <- combined_ouest %>%
  select(survey_region_name = area_name, geometry) %>%
  mutate(survey_id = "HTI2012DHS", REGVAR = "hv024", survey_region_id = 1,
         survey_region_name = tolower(survey_region_name))

#' Check that combined area is nested within survey boundaries
ggplot() +
  geom_sf(data = survey_region_boundaries %>%
            filter(survey_id == "HTI2006DHS")) +
  geom_sf(data = ouest_2006,
          aes(fill = survey_region_name),
          alpha = 0.3, color = NA) +
  ggtitle("Combined Ouest area vs. 2006 survey boundaries")

ggplot() +
  geom_sf(data = survey_region_boundaries %>%
            filter(survey_id == "HTI2012DHS")) +
  geom_sf(data = ouest_2012,
          aes(fill = survey_region_name),
          alpha = 0.3, color = NA) +
  ggtitle("Combined Ouest area vs. 2012 survey boundaries")

survey_region_boundaries <- rbind(survey_region_boundaries,
                                  ouest_2006,
                                  ouest_2012) %>%
  filter(!(survey_id == "HTI2006DHS" & survey_region_id == 9999)) %>%
  filter(!(survey_id == "HTI2012DHS" & survey_region_id == 9999)) %>%
  arrange(survey_id, survey_region_id)


#' Check thats survey regions align with HR datasets
hr06 <- readRDS(get_datasets("HTHR52FL.ZIP")[[1]])
count(hr06, hv024, as_factor(hv024))

survey_region_boundaries %>%
  filter(survey_id == "HTI2006DHS") %>%
  arrange(survey_region_id) %>%
  print(n = Inf)

hr12 <- readRDS(get_datasets("HTHR61FL.ZIP")[[1]])
count(hr12, hv024, as_factor(hv024))

survey_region_boundaries %>%
  filter(survey_id == "HTI2012DHS") %>%
  arrange(survey_region_id) %>%
  print(n = Inf)

#' Allocate each area to survey region
survey_region_areas <- allocate_areas_survey_regions(areas_wide, survey_region_boundaries)
validate_survey_region_areas(survey_region_areas, survey_region_boundaries, warn = TRUE)

survey_regions <- create_survey_regions_dhs(survey_region_areas)


#' Survey clusters dataset
survey_clusters <- create_survey_clusters_dhs(surveys)

#' Remove surveyed camps from clusters dataset
survey_clusters <- survey_clusters %>%
  filter(survey_region_id != 11)

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
while (!is.null(dev.list()))  dev.off()

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
  age_group_include = age_group_include
)

#' Save survey indicator dataset
write_csv(survey_indicators, "hti_survey_indicators_sexbehav.csv", na = "")

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
write_csv(hiv_indicators, "hti_hiv_indicators_sexbehav.csv", na = "")

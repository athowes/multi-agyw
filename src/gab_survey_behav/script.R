#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("gab_survey_behav")
# setwd("src/gab_survey_behav")

#' ISO3 country code
iso3 <- "GAB"

areas <- read_sf("depends/gab_areas.geojson") %>% st_make_valid()
areas_wide <- naomi::spread_areas(areas)

surveys <- create_surveys_dhs(iso3) %>%
  filter(as.numeric(SurveyYear) > 1998)

survey_meta <- create_survey_meta_dhs(surveys)

survey_region_boundaries <- create_survey_boundaries_dhs(surveys)

#' GAB2019DHS: Discrepancy in region codes between survey boundaries dataset
#' and survey datasets.
gha2019dhs_hr <- readRDS(get_datasets("GAHR71FL.ZIP")[[1]])
gha2019dhs_ge <- readRDS(get_datasets("GAGE71FL.ZIP")[[1]])

attr(gha2019dhs_hr$hv024, "labels")

gha2019dhs_ge %>% as.data.frame() %>%
  group_by(ADM1NAME, ADM1DHS) %>%
  summarise() %>%
  arrange(ADM1DHS)

survey_region_boundaries %>% group_by(survey_region_id, survey_region_name) %>%
  filter(survey_id == "GAB2019DHS") %>%
  summarise() %>% st_drop_geometry()

# Recode boundaries

# Area name            HR/GE  Boundaries
# Libreville            1    1
# Port-Gentil           2    2
# Estuaire              3    2
# Haut-Ogooue           4    3
# Moyen-Ogooue          5    4
# Ngounie               6    5
# Nyanga                7    6
# Ogooue-Ivindo         8    8
# Ogooue-Lolo           9    9
# Ogooue-Maritime      10    7
# Woleu-Ntem           11    10

survey_region_boundaries <- survey_region_boundaries %>%
  mutate(
    survey_region_id = case_when(
      survey_id == "GAB2019DHS" & survey_region_name == "estuaire" ~ 3,
      survey_id == "GAB2019DHS" & survey_region_name == "haut-ogooue" ~ 4,
      survey_id == "GAB2019DHS" & survey_region_name == "moyen-ogooue" ~ 5,
      survey_id == "GAB2019DHS" & survey_region_name == "ngounié" ~ 6,
      survey_id == "GAB2019DHS" & survey_region_name == "nyanga" ~ 7,
      survey_id == "GAB2019DHS" & survey_region_name == "ogooue-ivindo" ~ 8,
      survey_id == "GAB2019DHS" & survey_region_name == "ogooue-lolo" ~ 9,
      survey_id == "GAB2019DHS" & survey_region_name == "ogooue maritime" ~ 10,
      survey_id == "GAB2019DHS" & survey_region_name == "woleu-ntem" ~ 11,
      TRUE ~ survey_region_id
    )
  )


# survey_region_areas %>% group_by(survey_id, survey_region_id, survey_region_name)

surveys <- surveys_add_dhs_regvar(surveys, survey_region_boundaries)

#' Allocate each area to survey region
survey_region_areas <- allocate_areas_survey_regions(areas_wide, survey_region_boundaries)

# validate_survey_region_areas(survey_region_areas, survey_region_boundaries,
#                              warn = TRUE)
## Survey regions contained no areas:
##   survey_id survey_region_id     survey_region_name
##  GAB2012DHS                1 libreville-port-gentil
# GAB2019DHS                1             libreville
# GAB2019DHS                2            port-gentil

#' Manually add the districts that intersect libreville-port-gentil
#' survey region to survey_region_areas

gab2012dhs_region1_areas <- areas_wide %>%
  st_join(
    survey_region_boundaries %>%
      filter(survey_id == "GAB2012DHS", survey_region_id == 1),
    left = FALSE
  ) %>%
  select(all_of(names(survey_region_areas)))

gab2019dhs_region1_areas <- areas_wide %>%
  st_join(
    survey_region_boundaries %>%
      filter(survey_id == "GAB2019DHS", survey_region_id == 1),
    left = FALSE
  ) %>%
  select(all_of(names(survey_region_areas)))

gab2019dhs_region2_areas <- areas_wide %>%
  st_join(
    survey_region_boundaries %>%
      filter(survey_id == "GAB2019DHS", survey_region_id == 2),
    left = FALSE
  ) %>%
  select(all_of(names(survey_region_areas)))

# Missing areas for 2012DHS
# ggplot() +
#   geom_sf(data = gab2012dhs_region1_areas) +
#   geom_sf(data = survey_region_boundaries %>%
#             filter(survey_id == "GAB2012DHS", survey_region_id == 1), fill = NA, colour = "red")
#
# # Missing areas for 2019DHS
# ggplot() +
#   geom_sf(data = gab2019dhs_region1_areas) +
#   geom_sf(data = gab2019dhs_region2_areas) +
#   geom_sf(data = survey_region_boundaries %>%
#             filter(survey_id == "GAB2019DHS", survey_region_id %in% 1:2), fill = NA, colour = "red")

#  Add missing areas
survey_region_areas <- survey_region_areas %>%
  bind_rows(gab2012dhs_region1_areas, gab2019dhs_region1_areas, gab2019dhs_region2_areas)


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
write_csv(survey_indicators, "gab_survey_indicators_sexbehav.csv", na = "")

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
write_csv(hiv_indicators, "gab_hiv_indicators_sexbehav.csv", na = "")

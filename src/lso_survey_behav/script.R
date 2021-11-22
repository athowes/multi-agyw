#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("lso_survey_behav")
# setwd("src/lso_survey_behav")

#' ISO3 country code
iso3 <- "LSO"

#' Load area hierarchy
areas <- read_sf("depends/lso_areas.geojson")
areas_wide <- naomi::spread_areas(areas)

surveys <- create_surveys_dhs(iso3)
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
individuals <- create_individual_hiv_dhs(surveys)
names(individuals)

#' Extract the individual characteristics from the survey
survey_individuals <- create_survey_individuals_dhs(individuals)
names(survey_individuals)

#' Extract the HIV related characteristics from the survey
survey_biomarker <- create_survey_biomarker_dhs(individuals)
names(survey_biomarker)

#' Extract sexual behaviour characteristics from the survey
#' giftsvar is a special case indicator used to determine the type of survey question
survey_sexbehav <- create_sexbehav_dhs(surveys) %>%
  select(-giftsvar)
names(survey_sexbehav)

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
  age_group_include = age_group_include,
  area_bottom_level = 1
)

#' Save survey indicators dataset
write_csv(survey_indicators, "lso_survey_indicators_sexbehav.csv", na = "")

#' PHIA data from Jeff's reports
phia_survey_meta <- read_csv("depends/lso2017phia_survey_meta.csv")
phia_survey_regions <- read_csv("depends/lso2017phia_survey_regions.csv")
phia_survey_clusters <- read_csv("depends/lso2017phia_survey_clusters.csv")
phia_survey_individuals <- read_csv("depends/lso2017phia_survey_individuals.csv")
phia_survey_biomarker <- read_csv("depends/lso2017phia_survey_biomarker.csv")

#' Sexual behaviour PHIA data
#'
#' * sexbehav_time_intercourse:	(integer)	Time since last intercourse (imputed)
#' * sexbehav_age: (integer) Age at first intercourse (imp)
#' * sexbehav_partner_num: (integer) No. had sex including husband in last 12 months
#' * sexbehav_relationship:	(character)	Relationship with last sex partner
#' * sexbehav_other1: (character)	Relationship with other sex partner (1)
#' * sexbehav_other2: (character)	Relationship with other sex partner (2)
#' * sexbehav_sti: (integer) Had any STD in last 12 months
#' * sexbehav_sore:	(integer)	Had genital sore/ulcer in last 12 months
#' * sexbehav_discharge: (integer)	Had genital discharge in last 12 months
#' * sexbehav_marital: (integer) Current marital status
#' * sexbehav_condom:	(integer)	Condom used during last sex with most recent partner

phia_survey_sexbehav <- read_csv("depends/phia_sexbehav.csv") %>%
  filter(substr(survey_id, 1, 3) == iso3)

#' Add categories
phia_survey_sexbehav <- phia_survey_sexbehav %>%
  mutate(
    #' Ever had sex
    eversex = case_when(
      sexbehav_age == 0 ~ FALSE,
      sexbehav_age > 0 ~ TRUE,
      sexbehav_partner_num > 0 ~ TRUE, #' Sex in past year implies eversex
      is.na(sexbehav_age) ~ TRUE
    ),
    #' Reports sexual activity in the last 12 months
    sex12m = case_when(
      sexbehav_partner_num == 0 ~ FALSE,
      sexbehav_partner_num > 0 ~ TRUE,
      is.na(sexbehav_partner_num) ~ NA
    ),
    #' Does not report sexual activity in the last 12 months
    nosex12m = case_when(
      sex12m ~ FALSE,
      !sex12m ~ TRUE
    ),
    #' Reports sexual activity with exactly one cohabiting partner in the past 12 months
    sexcohab = case_when(
      eversex == FALSE ~ FALSE,
      sexbehav_partner_num == 1 & ((!sexbehav_other1 %in% 3:8) & (!sexbehav_other2 %in% 3:8)) ~ TRUE,
      is.na(sexbehav_partner_num) ~ NA,
      TRUE ~ FALSE
    ),
    #' Reports one or more non-regular sexual partner
    sexnonreg = case_when(
      eversex == FALSE ~ FALSE,
      sexbehav_partner_num > 1 ~ TRUE,
      (sexbehav_other1 %in% 3:8 | sexbehav_other2 %in% 3:8) ~ TRUE,
      is.na(sexbehav_partner_num) ~ NA,
      TRUE ~ FALSE
    ),
    #' Reports having exchanged gifts, cash, or anything else for sex in the past 12 months
    sexpaid12m = case_when(
      (sexbehav_other1 %in% 6:7 | sexbehav_other2 %in% 6:7) ~ TRUE,
      TRUE ~ FALSE
    ),
    #' Either sexnonreg or sexpaid12m
    sexnonregplus = ifelse(sexpaid12m, TRUE, sexnonreg),
    #' Just want the highest risk category that an individual belongs to
    nosex12m = ifelse(sexcohab | sexnonreg | sexpaid12m, FALSE, nosex12m),
    sexcohab = ifelse(sexnonreg | sexpaid12m, FALSE, sexcohab),
    sexnonreg = ifelse(sexpaid12m, FALSE, sexnonreg),
    #' Turn everything from TRUE / FALSE coding to 1 / 0
    across(eversex:sexnonregplus, ~ as.numeric(.x))
  )

#' PHIA survey indicator dataset
phia_survey_indicators <- calc_survey_indicators(
  phia_survey_meta,
  phia_survey_regions,
  phia_survey_clusters,
  phia_survey_individuals,
  phia_survey_biomarker,
  survey_other = NULL,
  st_drop_geometry(areas),
  sex = sex,
  age_group_include = age_group_include,
  area_bottom_level = 1
)

#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("mwi_data_survey_behav")
# setwd("src/mwi_data_survey_behav")

#' ISO3 country code
iso3 <- "MWI"

#' The spread_areas function "spreads area hierarchy to wide format"
areas <- read_sf("depends/mwi_areas.geojson")
areas_wide <- spread_areas(areas)

#' Filter all of the DHS with the corresponding iso3 code to the most recent survey
#' and obtain the meta data
surveys <- create_surveys_dhs(iso3)
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
  distinct(survey_id = "MWI2015DHS",
           survey_region_id = shdist,
           survey_region_name = as_factor(shdist))

#' All of the rows of hr15dhs_regions without a match in survey_region_boundaries (107, 210, 314, 315)
anti_join(hr15dhs_regions,
          survey_region_boundaries,
          by = c("survey_id", "survey_region_id"))

metro_boundaries <- areas %>%
  filter(area_level == 5,
         grepl("City$", area_name)) %>%
  select(area_id, area_name)

survey_region_id_recode <- c("MWI_5_08" = 107,
                             "MWI_5_18" = 210,
                             "MWI_5_25" = 314,
                             "MWI_5_33" = 315)

survey_region_name_recode <- c("MWI_5_08" = "Mzuzu City",
                               "MWI_5_18" = "Lilongwe City",
                               "MWI_5_25" = "Zomba City",
                               "MWI_5_33" = "Blantyre City")

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

#' # Survey clusters dataset
survey_clusters <- create_survey_clusters_dhs(surveys)

#' Snap survey clusters to areas
survey_clusters <- assign_dhs_cluster_areas(survey_clusters, survey_region_areas)

p_coord_check <- plot_survey_coordinate_check(survey_clusters,
                                              survey_region_boundaries,
                                              survey_region_areas)

dir.create("check")
pdf(paste0("check/", tolower(iso3), "_dhs-cluster-check.pdf"), h = 7, w = 6)
p_coord_check
dev.off()

#' Individual dataset
individuals <- create_individual_hiv_dhs(surveys)

survey_individuals <- create_survey_individuals_dhs(individuals)
survey_biomarker <- create_survey_biomarker_dhs(individuals)

survey_sexbehav <- create_sexbehav_dhs(surveys) %>%
  #' giftsvar is a special case indicator used to determine the type of survey question
  select(-giftsvar)

#' Remaining NA are all in eversex and sti12m
#' TODO: Add additional variables like age at first sex (in DHS, PHIA, MICS) to help resolve
survey_sexbehav %>% is.na() %>% colSums()

#' Verify no overlap
stopifnot(
  mutate(survey_sexbehav, r_sum = (1 - sex12m) + sexcohab + sexnonreg + sexpaid12m) %>%
    filter(r_sum != 1) %>% nrow() == 0
)

survey_other <- list(survey_sexbehav)

age_group_include <- c("Y015_019", "Y020_024", "Y025_029","Y015_024")
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

#' Save survey indicators dataset
write_csv(survey_indicators, "mwi_survey_indicators_sexbehav.csv", na = "")

#' Get prevalence estimates for different sexual behaviours
hiv_indicators <- calc_survey_hiv_indicators(
  survey_meta,
  survey_regions,
  survey_clusters,
  survey_individuals,
  survey_biomarker,
  #' Including sexnonregplus causes errors
  survey_other = list(survey_sexbehav %>% select(-sexnonregplus)),
  st_drop_geometry(areas),
  sex = sex,
  age_group_include = age_group_include,
  area_top_level = 0,
  area_bottom_level = 0,
  formula = ~ indicator + survey_id + area_id + res_type + sex + age_group +
    sex12m + eversex + sexcohab + sexnonreg + sexpaid12m + sti12m
)

#' Keep only the stratifications with "all" for others
hiv_indicators <- dplyr::bind_rows(dplyr::filter(hiv_indicators, eversex=="all" & sexcohab=="all" &
                                         sexnonreg=="all" & sexpaid12m=="all" & sti12m=="all" &
                                         (!is.na(sex12m) & sex12m!="all")),
                         dplyr::filter(hiv_indicators, sex12m=="all" & sexcohab=="all" &
                                         sexnonreg=="all" & sexpaid12m=="all" & sti12m=="all" &
                                         (!is.na(eversex) & eversex!="all")),
                         dplyr::filter(hiv_indicators, sex12m=="all" & eversex=="all" &
                                         sexnonreg=="all" & sexpaid12m=="all" & sti12m=="all" &
                                       (!is.na(sexcohab) & sexcohab!="all")),
                         dplyr::filter(hiv_indicators, sex12m=="all" & eversex=="all" &
                                         sexcohab=="all" & sexpaid12m=="all" & sti12m=="all" &
                                       (!is.na(sexnonreg) & sexnonreg!="all")),
                         dplyr::filter(hiv_indicators, sex12m=="all" & eversex=="all" &
                                         sexcohab=="all" & sexnonreg=="all" & sti12m=="all" &
                                       (!is.na(sexpaid12m) & sexpaid12m!="all")),
                         dplyr::filter(hiv_indicators, sex12m=="all" & eversex=="all" &
                                         sexcohab=="all" & sexnonreg=="all" & sexpaid12m=="all" &
                                       (!is.na(sti12m) & sti12m!="all")))

#' Save HIV indicators dataset
write_csv(hiv_indicators, "mwi_hiv_indicators_sexbehav.csv", na = "")

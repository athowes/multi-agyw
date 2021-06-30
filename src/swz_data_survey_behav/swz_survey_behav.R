#' ISO3 country code
iso3 <- "SWZ"

#' ## Load area hierarchy
areas <- read_sf("depends/swz_areas.geojson")
areas_wide <- naomi::spread_areas(areas)

surveys <- create_surveys_dhs(iso3)
surveys <- surveys %>% dplyr::filter(SurveyYear==max(SurveyYear))
survey_meta <- create_survey_meta_dhs(surveys)

survey_region_boundaries <- create_survey_boundaries_dhs(surveys)
surveys <- surveys_add_dhs_regvar(surveys, survey_region_boundaries)

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
pdf(paste0("check/", tolower(iso3), "_dhs-cluster-check.pdf"), h = 5, w = 7)
p_coord_check
dev.off()

#' # Individual dataset

individuals <- create_individual_hiv_dhs(surveys)

survey_individuals <- create_survey_individuals_dhs(individuals)
survey_biomarker <- create_survey_biomarker_dhs(individuals)

survey_sexbehav <- create_sexbehav_dhs(surveys)
survey_other <- list(survey_sexbehav)

age_group_include <- c("Y015_019","Y020_024", "Y025_029","Y015_024")
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
  age_group_include = age_group_include,
  area_bottom_level = 1
)

#' ## Save survey inidcators dataset
write_csv(survey_indicators, "swz_survey_indicators_sexbehav.csv", na = "")


#' ## Get prevalence estimates for different sexual behaviours

hiv_indicators <- calc_survey_hiv_indicators(
  survey_meta,
  survey_regions,
  survey_clusters,
  survey_individuals,
  survey_biomarker,
  survey_other,
  st_drop_geometry(areas),
  sex = sex,
  age_group_include = age_group_include,
  area_top_level = 0,
  area_bottom_level = 0,
  formula = ~ indicator + survey_id + area_id + res_type + sex + age_group +
    sex12m + eversex + sexcohab + sexnonreg + sexpaid12m + sti12m
)

#' ## Keep only the stratifications w/"all" for others

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

#' ## Save HIV inidcators dataset
write_csv(hiv_indicators, "swz_hiv_indicators_sexbehav.csv", na = "")



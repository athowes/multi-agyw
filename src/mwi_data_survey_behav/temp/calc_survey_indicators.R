#' Expand list of clusters at each area level
#'
#' This function recursively expands the list of clusters to produce a list
#' of survey clusters within areas at each level.
#'
#' @param survey_clusters survey clusters dataset
#' @param survey_regions survey regions dataset
#' @param areas area hierarchy dataset
#' @param top_level top level of the area hierarchy to expand (integer)
#' @param bottom_level bottom level of area hierarchy to expand (integer)
#'
#' TODO: These should be examples - where is areas_long.rds now?
#' areas_long <- readRDS(here::here("data/areas/areas_long.rds"))
#' survey_clusters <- readRDS(here::here("data/survey/survey_clusters.rds"))
#' survey_regions <- readRDS(here::here("data/survey/survey_regions.rds"))
#'
#' expand_survey_clusters(survey_clusters, areas_long)
#'
#' Get clusters at level 1 areas only
#' expand_survey_clusters(survey_clusters, areas_long, top_level = 1, bottom_level = 1)
#'
#' @noRd
#'
expand_survey_clusters <- function(survey_clusters,
                                   survey_regions,
                                   areas,
                                   top_level = min(areas$area_level),
                                   bottom_level = max(areas$area_level)) {

  if (!all(survey_clusters$geoloc_area_id %in% areas$area_id |
           is.na(survey_clusters$geoloc_area_id))) {
    stop("Survey cluster area id not in area hierarchy: ",
         paste0(setdiff(survey_clusters$geoloc_area_id, areas$area_id), collapse = ", "))
  }

  if (!all(survey_regions$survey_region_area_id %in% areas$area_id)) {
    stop("Survey region area id not in area hierarchy: ",
         paste0(setdiff(survey_regions$survey_region_area_id, areas$area_id), collapse = ", "))
  }

  clusters <- survey_clusters %>%
    dplyr::select(survey_id, cluster_id, res_type, survey_region_id, geoloc_area_id) %>%
    dplyr::left_join(
      survey_regions %>%
        dplyr::select(survey_id, survey_region_id, survey_region_area_id),
      by = c("survey_id", "survey_region_id")
    ) %>%
    dplyr::mutate(
      ## Get lowest known area for each cluster (geoloc or syvreg if not geocoded)
      area_id = dplyr::if_else(is.na(geoloc_area_id), survey_region_area_id, geoloc_area_id),
      geoloc_area_id = NULL,
      survey_region_area_id = NULL
    ) %>%
    dplyr::left_join(
      areas %>%
        dplyr::select(area_id, area_level, parent_area_id) %>%
        dplyr::arrange(area_id, -area_level) %>%
        dplyr::group_by(area_id) %>%
        dplyr::filter(dplyr::row_number() == 1),
      by = "area_id"
    )

  val <- clusters %>%
    dplyr::filter(dplyr::between(area_level, top_level, bottom_level))

  #' Recursion
  while(any(clusters$area_level >= top_level)) {

    clusters <- clusters %>%
      dplyr::mutate(area_id = parent_area_id,
                    area_level = area_level - 1L,
                    parent_area_id = NULL) %>%
      dplyr::inner_join(areas %>%
                          dplyr::select(area_id, area_level, parent_area_id),
                        by = c("area_id", "area_level"))

    val <- dplyr::bind_rows(
      val,
      clusters %>%
        dplyr::filter(dplyr::between(area_level, top_level, bottom_level))
    )
  }

  val$parent_area_id <- NULL

  return(val)
}



#' Calculate age/sex/area stratified survey estimates for all outcomes
#'
#' @details
#' All other data will be subsetted based on the `survey_id` values appearing in
#' survey_meta, so if only want to calculate for a subset of surveys it is
#' sufficient to pass subset for survey_meta and full data frames for the others.
#'
#' Much of this function needs to be parsed out into more generic functions and
#' rewritten to be more efficient.
#'   * Age group would be more efficient if traversing a tree structure.
#'   * Need generic function to calculate
#'   * Flexibility about age/sex stratifications to calculate.
#'
#' Not sure if passing "survey_other" as a list makes the most sense - depends how
#' the non-biomarker DHS datasets are prepped
#'
#' @param survey_meta Survey metadata.
#' @param survey_regions Survey regions.
#' @param survey_clusters Survey clusters.
#' @param survey_individuals Survey individuals.
#' @param survey_biomarker Survey biomarkers.
#' @param survey_other list of survey data at individual level (behavioural, etc).
#' @param areas Areas.
#' @param sex Sex.
#' @param age_group_include Vector of age agroups to include
#' @param area_top_level Area top level.
#' @param area_bottom_level Area bottom level.
#' @param artcov_definition Definition to use for calculate ART coverage.
#' @param by_res_type Whether to stratify estimates by urban/rural res_type; logical.
#' @param hiv_outcomes Whether to output only biomarker outcomes; logical.
#' @param by_hiv Whether to stratify non-HIV outcomes by HIV status; logical.
#'
#'
#' @details
#'
#' The argument `artcov_definition` controls whether to use both ARV biomarker and
#' self-report (`artcov_definition = "both"`; default), ARV biomarker only
#' (`artcov_definition = "arv"`), or self-report ART use only
#' (`artcov_definition = "artself"`).  If option is `"both"`, then all HIV positive
#' are used as the denomiator and no missing data on either indicator are
#' incorporated. If the option is `"arv"` or `"artself"` then missing values in those
#' variables, respectively, are treated as missing.
#'
#' @export
calc_survey_indicators <- function(survey_meta,
                                   survey_regions,
                                   survey_clusters,
                                   survey_individuals,
                                   survey_biomarker,
                                   survey_other,
                                   areas,
                                   sex = c("male", "female", "both"),
                                   age_group_include = NULL,
                                   area_top_level = min(areas$area_level),
                                   area_bottom_level = max(areas$area_level),
                                   by_res_type = FALSE,
                                   by_hiv = FALSE,
                                   formula = ~ indicator + survey_id + area_id + res_type + hiv_type + sex + age_group) {

  ## 1. Identify age groups to calculate for each survey_id
  age_groups <- naomi::get_age_groups()

  if(!is.null(age_group_include))
    age_groups <- dplyr::filter(age_groups, age_group %in% !!age_group_include)

  sex_age_group <- id_sex_age_groups(survey_meta, sex, age_groups)

  ## 2. Expand clusters to identify all clusters within each area
  clust_area <- id_clust_area(survey_meta, survey_regions, survey_clusters, areas,
                              area_top_level, area_bottom_level)

  ## 3. Expand individuals dataset to repeat for all individiuals within each
  ##    age/sex group for a given survey

  ## First concatenate list of all "other" survey data
  survey_other <- survey_other %>%
    Reduce(function(x,y) dplyr::inner_join(x, y, by= c("survey_id", "individual_id")), .)

  ind <- survey_individuals %>%
    dplyr::inner_join(survey_biomarker,
                      by = c("survey_id", "individual_id")) %>%
    dplyr::inner_join(survey_other,
                      by = c("survey_id", "individual_id")) %>%
    dplyr::filter(survey_id %in% survey_meta$survey_id)

  ## Data prep for non-biomarker outcomes
  ## for non-HIV outcomes, we're looking for all the variables from survey_other
  ## to make estimates for prevalence of behavioural/other indicators
  ## but we'll keep different variables if we want our behavioural
  ## indicators by HIV status or not - if we want by HIV status, only keep those
  ## with non-NA HIV status
  if(!by_hiv) {
    ind <- ind %>%
      dplyr::select(cluster_id, sex, age, indweight,
                    setdiff(names(survey_other),c("individual_id"))) %>%
      dplyr::rename(weights = indweight)
  } else {
    ind <- ind %>%
      dplyr::filter(!is.na(hivstatus)) %>%
      dplyr::select(cluster_id, sex, age, hivweight, hivstatus,
                    setdiff(names(survey_other),c("individual_id"))) %>%
      dplyr::rename(weights = hivweight)
  }

  ind <- ind %>%
    dplyr::bind_rows({.} %>% dplyr::mutate(sex = "both")) %>%
    dplyr::inner_join(sex_age_group, by = c("survey_id", "sex")) %>%
    dplyr::filter(age >= age_group_start,
                  age < age_group_start + age_group_span)

  ## 4. Join expanded age/sex and expanded cluster area map

  ind <- dplyr::inner_join(ind, clust_area, by = c("survey_id", "cluster_id"))

  ## Include res_type stratified and all if by_res_type is TRUE, otherwise recode to all
  if(by_res_type)
    ind <- dplyr::bind_rows(ind, dplyr::mutate(ind, res_type = "all"))
  else
    ind <- dplyr::mutate(ind, res_type = "all")

  ## Include hiv_type stratified and all if by_hiv is TRUE, otherwise recode to all
  ## if by_hiv is TRUE, filtering out anyone without an HIV status...
  if(by_hiv) {
    ind$hiv_type <- dplyr::case_when(ind$hivstatus == 1 ~ "positive",
                                     ind$hivstatus == 0 ~ "negative",
                                     TRUE ~ NA_character_)
    ind <- dplyr::bind_rows(ind, dplyr::mutate(ind,hiv_type = ifelse(!is.na(hivstatus), "all", NA)))
  } else {
    ind <- dplyr::mutate(ind, hiv_type = "all")
  }

  ## 5. Pivot to long format
  ind <- ind %>%
    tidyr::pivot_longer(
      cols = setdiff(names(survey_other),c("survey_id","individual_id")),
      names_to = "indicator",
      values_to = "estimate"
    ) %>%
    dplyr::filter(!is.na(estimate))


  ## 6. Calculate outcomes
  ## Note: using survey region as strata right now. Most DHS use region + res_type
  group_by_vars <- c("indicator", "survey_id", "area_id", "res_type","hiv_type", "sex", "age_group")
  split_vars <- c("indicator", "survey_id", "area_level", "res_type","hiv_type", "sex", "age_group")
  extra_vars <- NULL

  val <- calc_all_outcomes(ind, group_by_vars, split_vars,
                           formula = formula , extra_vars,
                           survey_meta, areas, age_groups)

  val
}

#' Calculate age/sex/area stratified survey estimates for HIV biomarker outcomes
#'
#' @details
#' All other data will be subsetted based on the `survey_id` values appearing in
#' survey_meta, so if only want to calculate for a subset of surveys it is
#' sufficient to pass subset for survey_meta and full data frames for the others.
#'
#' Much of this function needs to be parsed out into more generic functions and
#' rewritten to be more efficient.
#'   * Age group would be more efficient if traversing a tree structure.
#'   * Need generic function to calculate
#'   * Flexibility about age/sex stratifications to calculate.
#'
#' Not sure if passing "survey_other" as a list makes the most sense - depends how
#' the non-biomarker DHS datasets are prepped
#'
#' @param survey_meta Survey metadata.
#' @param survey_regions Survey regions.
#' @param survey_clusters Survey clusters.
#' @param survey_individuals Survey individuals.
#' @param survey_biomarker Survey biomarkers.
#' @param survey_other list of survey data at individual level (behavioural, etc).
#' @param areas Areas.
#' @param sex Sex.
#' @param age_group_include Vector of age agroups to include
#' @param area_top_level Area top level.
#' @param area_bottom_level Area bottom level.
#' @param artcov_definition Definition to use for calculate ART coverage.
#' @param by_res_type Whether to stratify estimates by urban/rural res_type; logical.
#' @param hiv_outcomes Whether to output only biomarker outcomes; logical.
#' @param by_hiv Whether to stratify non-HIV outcomes by HIV status; logical.
#'
#'
#' @details
#'
#' The argument `artcov_definition` controls whether to use both ARV biomarker and
#' self-report (`artcov_definition = "both"`; default), ARV biomarker only
#' (`artcov_definition = "arv"`), or self-report ART use only
#' (`artcov_definition = "artself"`).  If option is `"both"`, then all HIV positive
#' are used as the denomiator and no missing data on either indicator are
#' incorporated. If the option is `"arv"` or `"artself"` then missing values in those
#' variables, respectively, are treated as missing.
#'
#' @export
calc_survey_hiv_indicators <- function(survey_meta,
                                       survey_regions,
                                       survey_clusters,
                                       survey_individuals,
                                       survey_biomarker,
                                       survey_other = NULL,
                                       areas,
                                       sex = c("male", "female", "both"),
                                       age_group_include = NULL,
                                       area_top_level = min(areas$area_level),
                                       area_bottom_level = max(areas$area_level),
                                       artcov_definition = c("both", "arv", "artself"),
                                       by_res_type = FALSE,
                                       formula = ~ indicator + survey_id + area_id + res_type + sex + age_group) {

  ## 1. Identify age groups to calculate for each survey_id
  ## Identify age groups to calculate for each survey_id
  age_groups <- naomi::get_age_groups()

  if(!is.null(age_group_include))
    age_groups <- dplyr::filter(age_groups, age_group %in% !!age_group_include)

  sex_age_group <- id_sex_age_groups(survey_meta, sex, age_groups)

  ## 2. Expand clusters to identify all clusters within each area
  clust_area <- id_clust_area(survey_meta, survey_regions, survey_clusters, areas,
                              area_top_level, area_bottom_level)

  ## 3. Expand individuals dataset to repeat for all individiuals within each
  ##    age/sex group for a given survey
  ind <- survey_individuals %>%
    dplyr::inner_join(survey_biomarker,
                      by = c("survey_id", "individual_id")) %>%
    dplyr::filter(survey_id %in% survey_meta$survey_id)

  # join in the other data if we have any
  if(!is.null(survey_other)) {
    ## First concatenate list of all "other" survey data
    survey_other <- survey_other %>%
      Reduce(function(x,y) dplyr::inner_join(x, y, by= c("survey_id", "individual_id")), .)

    ind <- ind %>%
      dplyr::inner_join(survey_other,
                        by = c("survey_id", "individual_id"))
    # others will tell us the names of variables to keep below
    others <- setdiff(names(survey_other),c("individual_id","survey_id"))
  } else { others <- NULL }

  ## Only keep the relevant variables for biomarkers, and drop folks without HIV-status
  ## if we have other variables, keep these as well
  ind <- ind %>%
    dplyr::filter(!is.na(hivstatus)) %>%
    dplyr::select(survey_id, cluster_id, sex, age, hivweight, hivstatus, artself, arv, vls, recent, all_of(others)) %>%
    dplyr::rename(weights = hivweight)

  ## Use the sex_age_group dataframe to filter to the ages we care about
  ind <- ind %>%
    dplyr::bind_rows({.} %>% dplyr::mutate(sex = "both")) %>%
    dplyr::inner_join(sex_age_group, by = c("survey_id", "sex")) %>%
    dplyr::filter(age >= age_group_start,
                  age < age_group_start + age_group_span)

  ## 4. Join expanded age/sex and expanded cluster area map

  ind <- dplyr::inner_join(ind, clust_area, by = c("survey_id", "cluster_id"))

  ## Include res_type stratified and all if by_res_type is TRUE, otherwise recode to all
  if(by_res_type)
    ind <- dplyr::bind_rows(ind, dplyr::mutate(ind, res_type = "all"))
  else
    ind <- dplyr::mutate(ind, res_type = "all")

  ## Include "other" vars stratified and all if !is.null(others)
  if(!is.null(others)) {
    for(i in 1:length(others)) {
      ind[,names(ind) %in% others[i]] <- as.character(ind[,names(ind) %in% others[i]])
      ind_ext <- ind
      ind_ext[,names(ind_ext) %in% others[i]] <- "all"
      ind <- dplyr::bind_rows(ind, ind_ext)
    }
  }

  ## 5. Construct ART coverage indicator as either self-report or ART biomarker
  ##    and gather to long dataset for each biomarker

  if(artcov_definition[1] == "both") {
    ind <- ind %>%
      dplyr::group_by(survey_id) %>%
      dplyr::mutate(has_artcov = any(!is.na(arv) | !is.na(artself)),
                    artcov = dplyr::case_when(!has_artcov ~ NA_integer_,
                                              hivstatus == 0 ~ NA_integer_,
                                              arv == 1 | artself == 1 ~ 1L,
                                              TRUE ~ 0L)) %>%
      dplyr::select(-has_artcov, -artself, -arv) %>%
      dplyr::ungroup()
  } else if(artcov_definition[1] %in% c("arv", "artself")) {
    ind <- dplyr::rename(ind, artcov = artcov_definition[1]) %>%
      dplyr::mutate(artcov = dplyr::if_else(hivstatus == 0,  NA_integer_, as.integer(artcov)))
  } else {
    stop(paste("Invalid artcov_definition value:", artcov_definition[1]))
  }

  ## Rename variables to outcome indicators
  ind <- ind %>%
    dplyr::rename(prevalence = hivstatus,
                  art_coverage = artcov,
                  viral_suppression_plhiv = vls,
                  recent_infected = recent)

  ## Pivot to long format
  ind <- ind %>%
    tidyr::pivot_longer(
      cols = c(prevalence, art_coverage, viral_suppression_plhiv, recent_infected),
      names_to = "indicator",
      values_to = "estimate"
    ) %>%
    dplyr::filter(!is.na(estimate))


  ## 6. Calculate outcomes
  ## Note: using survey region as strata right now. Most DHS use region + res_type
  group_by_vars <- c("indicator", "survey_id", "area_id", "res_type", "sex", "age_group",others)
  split_vars <- c("indicator", "survey_id", "area_level", "res_type", "sex", "age_group",others)
  extra_vars <- others


  val <- calc_all_outcomes(ind, group_by_vars, split_vars,
                           formula = ~ indicator + survey_id + area_id + res_type + sex + age_group +
                             sex12m + eversex + sexcohab + sexnonreg + sexpaid12m + sti12m ,
                           extra_vars, survey_meta, areas, age_groups)

  val
}

#' Calculate ...

calc_all_outcomes <- function(ind,
                              group_by_vars,
                              split_vars,
                              formula,
                              extra_vars,
                              survey_meta,
                              areas,
                              age_groups) {
  dat <- ind %>%
    dplyr::filter(!is.na(weights), weights > 0)

  cnt <- dat %>%
    dplyr::group_by_at(group_by_vars) %>%
    dplyr::summarise(n_clusters = dplyr::n_distinct(cluster_id),
                     n_observations = dplyr::n(),
                     n_eff_kish = sum(weights)^2 / sum(weights^2),
                     .groups = "drop")

  dat$spl <- apply(dplyr::select(dat,all_of(split_vars)),1,paste,collapse=" ")

  ## Find better way to drop the NA's for the variable without dropping all NA's prior
  ## to inputting
  dat <- dat %>% filter(!grepl(" NA",substr(spl,15,1000)))

  datspl <- dat %>% split(.$spl)

  do_svymean <- function(df,
                         formula = ~ indicator + survey_id + area_id + res_type + sex + age_group) {

    des <- survey::svydesign(~cluster_id,
                             data = df,
                             strata = ~survey_id + survey_region_id,
                             nest = TRUE,
                             weights = ~weights)

    val <- survey::svyby(~estimate,
                         formula,
                         des, survey::svymean)
    names(val)[names(val) == "se"] <- "std_error"
    val
  }

  options(survey.lonely.psu="adjust")
  mc.cores <- if(.Platform$OS.type == "windows") 1 else parallel::detectCores()
  est_spl <- parallel::mclapply(datspl, do_svymean, formula=formula, mc.cores = mc.cores)

  val <- cnt %>%
    dplyr::full_join(
      dplyr::bind_rows(est_spl),
      by = group_by_vars
    ) %>%
    dplyr::left_join(
      survey_meta %>% dplyr::select(survey_id, survey_mid_calendar_quarter),
      by = c("survey_id")
    ) %>%
    dplyr::left_join(
      areas %>%
        dplyr::select(area_id, area_name, area_level, area_sort_order),
      by = c("area_id")
    ) %>%
    dplyr::left_join(
      dplyr::select(age_groups, age_group, age_group_sort_order),
      by = "age_group"
    ) %>%
    dplyr::arrange(
      factor(indicator, c("prevalence", "art_coverage", "viral_suppression_plhiv", "recent_infected")),
      survey_id,
      survey_mid_calendar_quarter,
      area_level,
      area_sort_order,
      area_id,
      factor(res_type, c("all", "urban", "rural")),
      # factor(hiv_type, c("all", "positive", "negative")),
      factor(sex, c("both", "male", "female")),
      age_group_sort_order
    ) %>%
    dplyr::select(
      indicator,
      survey_id,
      survey_mid_calendar_quarter,
      area_id,
      area_name,
      res_type,
      all_of(extra_vars),
      sex,
      age_group,
      n_clusters,
      n_observations,
      n_eff_kish,
      estimate,
      std_error
    ) %>%
    dplyr::distinct()

  ## Calculate 95% CI on logit scale
  val <- val %>%
    dplyr::mutate(
      ci_lower = calc_logit_confint(estimate, std_error, "lower"),
      ci_upper = calc_logit_confint(estimate, std_error, "upper")
    )
  val
}

#' Identify age groups to calculate for each survey_id
#'
#' @param survey_meta Survey metadata.
#' @param sex Sex.
#' @param age_group_include Vector of age agroups to include
#'
#' @return data frame with the unique sex/age-group combinations to be outputted
#'
#' @noRd
id_sex_age_groups <- function(survey_meta,
                              sex,
                              age_groups) {

  sex_age_group <- tidyr::crossing(sex, age_groups)

  ## Only keep age groups that are fully contained within survey age range.
  ## For example, if survey sampled age 18-64, don't want to calculate
  ## aggregates for age 15-49.

  sex_age_group <- survey_meta %>%
    dplyr::select(survey_id, female_age_min, female_age_max,
                  male_age_min, male_age_max) %>%
    tidyr::crossing(sex_age_group) %>%
    dplyr::filter(age_group_start >= dplyr::case_when(sex == "male" ~ male_age_min,
                                                      sex == "female" ~ female_age_min,
                                                      sex == "both" ~ pmin(male_age_min,
                                                                           female_age_min)),
                  age_group_start + age_group_span <= dplyr::case_when(sex == "male" ~ male_age_max,
                                                                       sex == "female" ~ female_age_max,
                                                                       sex == "both" ~ pmin(male_age_max,
                                                                                            female_age_max)) + 1) %>%
    dplyr::select(survey_id, sex, age_group, age_group_label, age_group_start, age_group_span)

  sex_age_group
}

#'   Expand clusters to identify all clusters within each area
#'
#' @param survey_meta Survey metadata.
#' @param survey_regions Survey regions.
#' @param survey_clusters Survey clusters.
#' @param areas Areas.
#' @param area_top_level Area top level.
#' @param area_bottom_level Area bottom level.
#'
#' @return grouped data frame with the survey clusters and each area_id/are_level
#' corresponding to the cluster
#'
#' @noRd
id_clust_area <- function(survey_meta,
                          survey_regions,
                          survey_clusters,
                          areas,
                          area_top_level,
                          area_bottom_level) {
  ## 2. Expand clusters to identify all clusters within each area

  clust <- dplyr::filter(survey_clusters, survey_id %in% survey_meta$survey_id)
  clust_area <- expand_survey_clusters(clust, survey_regions, areas,
                                       area_top_level, area_bottom_level)

  clust_area <- clust_area %>%
    dplyr::arrange(survey_id, cluster_id, area_id, -area_level) %>%
    dplyr::group_by(survey_id, cluster_id, area_id) %>%
    dplyr::filter(dplyr::row_number() == 1)

  clust_area
}

calc_logit_confint <- function(estimate, std_error, tail, conf.level = 0.95) {

  stopifnot(length(estimate) == length(std_error))
  stopifnot(tail %in% c("lower", "upper"))
  stopifnot(conf.level > 0 & conf.level < 1)

  crit <- qnorm(1 - (1 - conf.level) / 2) * switch(tail, "lower" = -1, "upper" = 1)
  lest <- stats::qlogis(estimate)
  lest_se <- std_error / (estimate * (1 - estimate))

  ifelse(estimate < 1 & estimate > 0, stats::plogis(lest + crit * lest_se), NA_real_)
}

#' Find Calendar Quarter Midpoint of Two Dates
#'
#' @param start_date vector coercibel to Date
#' @param end_date vector coercibel to Date
#'
#' @return A vector of calendar quarters
#'
#' @examples
#' start <- c("2005-04-01", "2010-12-01", "2016-01-01")
#' end <-c("2005-08-01", "2011-05-01", "2016-06-01")
#'
#' mid_calendar_quarter <- get_mid_calendar_quarter(start, end)
#'
#' @export
get_mid_calendar_quarter <- function(start_date, end_date) {

  start_date <- lubridate::decimal_date(as.Date(start_date))
  end_date <- lubridate::decimal_date(as.Date(end_date))

  stopifnot(!is.na(start_date))
  stopifnot(!is.na(end_date))
  stopifnot(start_date <= end_date)

  date4 <- (start_date + end_date) / 2
  year <- floor(date4)
  quarter <- floor((date4 %% 1) * 4) + 1

  paste0("CY", year, "Q", quarter)
}

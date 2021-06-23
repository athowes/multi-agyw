areas <- st_drop_geometry(areas)
area_top_level <- min(areas$area_level)
area_bottom_level <- max(areas$area_level)
area_bottom_level <- 5
by_res_type <- FALSE
by_hiv <- FALSE
formula <- ~ indicator + survey_id + area_id + res_type + hiv_type + sex + age_group

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

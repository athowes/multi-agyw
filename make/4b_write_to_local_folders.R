### INTEGRATE W/NAOMI ESTIMATES PROCESS

## WRITE THE FILES TO INDIVIDUAL FOLDERS IN GITHUB

write_to_file <- function(file,
                          outdir = "~/Documents/GitHub/naomi.resources/inst/extdata/shipp",
                          sex) {
  df <- readr::read_csv(file, show_col_types = FALSE) %>%
    dplyr::select(iso3, survey_id, indicator, area_id, area_name, area_sort_order, year, age_group,
                  estimate_smoothed)
  dat <- split(df, df$iso3)
  isos <- names(dat)
  # Create folder if it doesnt exist
  for(x in isos){
    if(length(list.files(file.path(outdir, x))) == 0 ){
      dir.create(file.path(outdir, x))
    }
    # Save estimates into folder
    print(x)
    filename <- file.path(outdir, x, paste0(sex, "_best-3p1-multi-sexbehav-sae.csv"))
    readr::write_csv(dat[[x]],filename)
    print(paste0(filename, " saved to file"))
  }
}

# Set file path to csv with SAE results
write_to_file(file = "/Users/krisher/Documents/Copied Over/GitHub/multi-agyw/archive/process_differentiate-high-risk/20251121-221848-e5132cf4/best-3p1-multi-sexbehav-sae.csv",
              outdir = "~/Documents/GitHub/naomi.resources/inst/extdata/shipp",
              sex = "female")
write_to_file(file = "/Users/krisher/Documents/Copied Over/GitHub/multi-agyw/archive/process_differentiate-high-risk_men/20251121-233048-a6e8820b/best-3p1-multi-sexbehav-sae.csv",
              outdir = "~/Documents/GitHub/naomi.resources/inst/extdata/shipp",
              sex = "male")

## OUTPUT LOG ODDS RATIOS BY COUNTRY FOR NAOMI INTEGRATION
#' Format SRB survey estimates
#' ######## ALL COUNTRY ESTIMATE FOR CAF & BEN BECAUSE NO HIV DATA
#' ######## EDIT IF OTHER COUNTRIES IN THE SAME CASE

calculate_prevalence_lor <- function(iso3, srb_survey, sex){

  ##### IF CAF OR BEN KEEP ALL DATA
  ##### FIGURE OUT WHAT'S GOING ON WITH ZWE!!
  ##### FIGURE OUT WHAT'S GOING ON WITH ZAF & GMB MEN
  if(iso3=="CAF" | iso3=="BEN" | iso3=="ZWE" |
     (iso3=="ZAF" & sex=="male") | (iso3=="GMB" & sex=="male")) {
    prev_wide <- srb_survey %>%
      dplyr::filter(
        (nosex12m != 0) & (sexcohab != 0) & (sexnonreg != 0) & (sexpaid12m != 0),
        !age_group %in% c("Y015_024","Y015_049","Y025_049"),
        indicator == "prevalence") %>%
      dplyr::mutate(
        behav = dplyr::case_when(
          nosex12m == 1 ~ "nosex12m", sexcohab == 1 ~ "sexcohab",
          sexnonreg == 1 ~ "sexnonreg", sexpaid12m == 1 ~ "sexpaid12m",
          TRUE ~ "all"), .after = indicator) %>%
      dplyr::select(indicator, behav, survey_id, area_id, age_group, estimate) %>%
      tidyr::pivot_wider(
        names_from = "behav",
        values_from = "estimate")
  } else {
    prev_wide <- srb_survey %>%
      dplyr::filter(area_id == iso3) %>%
      dplyr::filter(
        (nosex12m != 0) & (sexcohab != 0) & (sexnonreg != 0) & (sexpaid12m != 0),
        !age_group %in% c("Y015_024","Y015_049","Y025_049"),
        indicator == "prevalence") %>%
      dplyr::mutate(
        behav = dplyr::case_when(
          nosex12m == 1 ~ "nosex12m", sexcohab == 1 ~ "sexcohab",
          sexnonreg == 1 ~ "sexnonreg", sexpaid12m == 1 ~ "sexpaid12m",
          TRUE ~ "all"), .after = indicator) %>%
      dplyr::select(indicator, behav, survey_id, area_id, age_group, estimate) %>%
      tidyr::pivot_wider(
        names_from = "behav",
        values_from = "estimate")
  }

  ind <- prev_wide %>%
    dplyr::mutate(
      #' Calculate the odds
      across(nosex12m:all, ~ .x / (1 - .x), .names = "{.col}_odds"),
      #' Log odds
      across(nosex12m:all, ~ log(.x / (1 - .x)), .names = "{.col}_logodds"),
      #' Prevalence ratios
      across(nosex12m:all, ~ .x / all, .names = "{.col}_pr"),
      #' Odds ratios
      across(nosex12m:all, ~ (.x / (1 - .x)) / all_odds, .names = "{.col}_or")
    ) %>%
    dplyr::rename_with(.cols = nosex12m:all, ~ paste0(.x, "_prevalence")) %>%
    dplyr::select(-indicator) %>%
    tidyr::pivot_longer(
      cols = starts_with(c("nosex12m", "sexcohab", "sexnonreg", "sexpaid12m", "all")),
      names_to = "indicator",
      values_to = "estimate"
    ) %>%
    tidyr::separate(indicator, into = c("behav", "indicator"))

  ind_dat <- ind %>%
    dplyr::mutate(
      nosex12m_id = ifelse(behav == "nosex12m", 1, 0),
      sexcohab_id = ifelse(behav == "sexcohab", 1, 0),
      sexnonreg_id = ifelse(behav == "sexnonreg", 1, 0),
      sexpaid12m_id = ifelse(behav == "sexpaid12m", 1, 0),
      all_id = ifelse(behav == "all", 1, 0),
      year = as.numeric(substr(survey_id,4,7))) %>%
    dplyr::filter(indicator == "prevalence", !is.na(estimate))

  #' Younger age groups regression
  fit_y <- glm(estimate ~ -1 + all_id + nosex12m_id + sexcohab_id + sexnonreg_id + sexpaid12m_id,
               family = quasibinomial(link = "logit"),
               data = ind_dat %>% dplyr::filter(age_group %in% c("Y015_019","Y020_024","Y025_029")))

  odds_estimate <- exp(fit_y$coefficients)
  or_y <- odds_estimate / odds_estimate[1]
  lor_y <- log(odds_estimate / odds_estimate[1])
  lor_y <- lor_y[-1]

  odds_estimate <- exp(fit_y$coefficients)
  or_y <- odds_estimate / odds_estimate[1]
  lor_y <- log(odds_estimate / odds_estimate[1])
  lor_y <- lor_y[-1]

  #' Older age groups regression
  fit_o <- glm(estimate ~ -1 + all_id + nosex12m_id + sexcohab_id + sexnonreg_id + sexpaid12m_id,
               family = quasibinomial(link = "logit"),
               data = ind_dat %>% dplyr::filter(age_group %in% c("Y030_034","Y035_039","Y040_044","Y045_49")))

  odds_estimate <- exp(fit_o$coefficients)
  or_o <- odds_estimate / odds_estimate[1]
  lor_o <- log(odds_estimate / odds_estimate[1])
  lor_o <- lor_o[-1]


  data.frame(iso3 = iso3 ,
             sex = sex,
             "lor_15to29" = lor_y,
             "lor_30to49" = lor_o,
             srb_group = names(lor_y))

}
#
# isos <- multi.utils::priority_iso3()

srb_survey <- readr::read_csv("~/Downloads/sae_outputs_updated/women/hiv_indicators_sexbehav.csv", show_col_types = FALSE)

isos <- unique(substr(srb_survey$area_id,1,3))

lor_female <- lapply(isos, calculate_prevalence_lor, srb_survey, "female") %>%
  bind_rows()

srb_survey <- readr::read_csv("~/Downloads/sae_outputs_updated/men/hiv_indicators_sexbehav.csv", show_col_types = FALSE)

lor_male <- lapply(isos, calculate_prevalence_lor, srb_survey, "male") %>%
  bind_rows()

# write_csv(lor_df, "lor_df_females.csv")
#
# lor_female <- readr::read_csv("srb-groups-lor-females.csv")
# lor_male <- readr::read_csv("srb-groups-lor-males.csv")
lor_df <- dplyr::bind_rows(lor_female, lor_male)
rownames(lor_df) <- NULL
# Write to naomi.resources
write_lor_tofile <-  function(iso, df) {
  path <- file.path("~/Documents/GitHub/mrc-ide/naomi.resources/inst/extdata/shipp/",
                    iso, "prevalence_lor.csv")
  df <- df %>% dplyr::filter(iso3==iso)
  readr::write_csv(df, path)
}
isos <- unique(lor_df$iso3)
lapply(isos, write_lor_tofile, df = lor_df)

# Commented this out since no update
# write_afs_to_file <- function(file,
#                           outdir = "~/Documents/GitHub/mrc-ide/naomi.resources/inst/extdata/shipp") {
#   df <- readRDS(file)
#   dat <- split(df, df$ISO_A3)
#   isos <- names(dat)
#   # Create folder if it doesnt exist
#   for(x in isos){
#     if(length(list.files(file.path(outdir, x))) == 0 ){
#       dir.create(file.path(outdir, x))
#     }
#     # Save estimates into folder
#     print(x)
#     filename <- file.path(outdir, x, "kinh_afs_dist.csv")
#     readr::write_csv(dat[[x]],filename)
#     print(paste0(filename, " saved to file"))
#   }
# }
#
# write_afs_to_file(file = "/Users/krisher/Documents/GitHub/multi-agyw/src/process_age-disagg-fsw/kinh-afs-dist.rds",
#               outdir = "~/Documents/GitHub/naomi.resources/inst/extdata/agyw")

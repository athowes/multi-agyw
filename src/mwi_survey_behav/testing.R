#' Script for testing new variable codings

#' Start of `create_sexbehav_dhs` function

surveys <- surveys

ird <- rdhs::dhs_datasets(fileType = "IR", fileFormat = "flat")
mrd <- rdhs::dhs_datasets(fileType = "MR", fileFormat = "flat")

ird <- dplyr::filter(ird, SurveyId %in% surveys$SurveyId)
mrd <- dplyr::filter(mrd, SurveyId %in% surveys$SurveyId)

# doesn't like this line unless the model_datasets data is loaded
ird_paths <- setNames(rdhs::get_datasets(ird), ird$SurveyId)

if (nrow(mrd) > 0) {
  mrd_paths <- setNames(rdhs::get_datasets(mrd), mrd$SurveyId)
} else {
  mrd_paths <- list(NULL)
}

#' Start of loop over `extract_sexbehav_dhs`

#' Input variables
#' Going for the 2015 survey, as it's close to the PHIA
SurveyId <- surveys$SurveyId[[4]]
ird_path <- ird_paths[surveys$SurveyId]
mrd_path <- mrd_paths[surveys$SurveyId]

#' Start `extract_sexbehav_dhs` function

message("Parsing IR/MR Sexual Behaviour datasets: ", SurveyId)

sb_vars <- c("v504", "v529", "v531", "v766b", "v767a", "v767b", "v767c", "v791a",
             "v763a", "v763b", "v763c", "v501")

## Individual recode
ir <- readRDS(ird_path[[SurveyId]])
dat <- dplyr::select(ir, individual_id = caseid, tidyselect::any_of(sb_vars))
dat[setdiff(sb_vars, names(dat))] <- NA

## Male recode
if (!is.null(mrd_path)) {
  spec_fvars <- which(sb_vars %in% c("v791a"))
  sb_mvars <- paste0("m", sb_vars[-spec_fvars])
  sb_mvars <- c(sb_mvars, "mv793")
  mr <- readRDS(mrd_path)
  mdat <- dplyr::select(mr, individual_id = mcaseid, tidyselect::any_of(sb_mvars))
  mdat[setdiff(sb_mvars, names(mdat))] <- NA
  names(mdat) <- sub("^mv", "v", names(mdat))

  dat <- dplyr::bind_rows(dat, mdat)
}

## # Code sexual behaviour outcomes

# # eversex = whether participant has ever had sex
# # Recode v531 - age of sexual debut - anyone coded 0 hasn't had sex,
# # 97 = inconsistent, 98 = Don't Know, 99 = Missing
# # Assume if you don't know the age of sexual debut that you have had sex
# # Should inconsistents be TRUE instead of missing?
# dat$eversex <- dplyr::case_when(dat$v531 %in% c(97,99) ~ NA,
#                                 dat$v531 == 98 ~ TRUE,
#                                 TRUE ~ dat$v531 > 0)

# sex12m = whether reports sexual activity in past 12 mo
# Recode v766b - number of partners in the past 12 mo
# use v527/v529 instead?
dat$sex12m <- dplyr::case_when(dat$v766b == 0 ~ FALSE,
                               dat$v766b == 99 ~ NA,
                               TRUE ~ dat$v766b > 0)

dat$nosex12m <- 1 - dat$sex12m

# sexcohab = whether reports sex with only one cohabiting partner in the past
# 12 mo.  Recode v766b (# partners in past 12 mo) and v767a-c (relationship
# w/partners)
# Currently if your partner type is missing or inconsistent and
# you had only one partner in the past year you are classified as a "yes"
# for having sex with only one cohabiting partner

# v767a-c coding does not match DHS questionnaire and isn't labelled - correct is:
# 1 = spouse, 2 = boyfriend not living with respondent, 3 = other friend
# 4 = casual acquaintance, 5 = relative, 6 = commercial sex worker,
# 7 = live-in partner, 96 = other
cas_cats <- c(2, 3, 4, 5, 6, 96)
dat$sexcohabspouse <- dplyr::case_when(dat$sex12m == FALSE ~ FALSE,
                                       dat$v766b == 1 & ((!dat$v767a %in% cas_cats) &
                                                           (!dat$v767b %in% cas_cats) &
                                                           (!dat$v767c %in% cas_cats)) ~ TRUE,
                                       dat$v766b == 99 ~ NA,
                                       TRUE ~ FALSE)

# A stricter version of sexcohabspouse
# v504 = whether the partner lives in the household or is now living elsewhere (for currently
# married or in union women):
# 1 = living with women
# 2 = living elsewhere
# 9 = missing
# NA = missing
dat$sexcohab <- dplyr::case_when(dat$sexcohabspouse == TRUE & dat$v504 == 1 ~ TRUE,
                                 TRUE ~ FALSE)

# sexnonreg = whether the person reports having non-regular sexual partner(s)
# or multiple partners in the past year. Recode v766b (# partners in past 12 mo)
# and v767a-c (relationship w/partners)
dat$sexnonreg <- dplyr::case_when(dat$sex12m == FALSE ~ FALSE,
                                  dat$v504 == 2 ~ TRUE,
                                  (dat$v766b > 1 & dat$v766b != 99) |
                                    (dat$v767a %in% cas_cats | dat$v767b %in% cas_cats |
                                       dat$v767c %in% cas_cats) ~ TRUE,
                                  dat$v766b == 99 ~ NA,
                                  TRUE ~ FALSE)

# sexpaid12m = whether the person reports having received gifts/cash/anything
# in exchange for sex (women aged 15-24, recode v791a or v767a-c), or paid for sex in the
# past 12 months (men, recode v793 and v767a-c)
# v791a is only collected if 15-24 yo woman has never been in a union - should probably
# make this var NA if woman is over 25 or if v501 (marital status) is missing
if(!is.null(mrd_path)) {
  dat$sexpaid12m <- dplyr::case_when(dat$v791a == 1 |
                                       (dat$v767a == 6 | dat$v767b == 6 |
                                          dat$v767c == 6) | dat$v793 == 1 ~ TRUE,
                                     (is.na(dat$v791a) & is.na(dat$v767a) & is.na(dat$v767b) &
                                        is.na(dat$v767c)) | (is.na(dat$v793) &
                                                               is.na(dat$v767a) & is.na(dat$v767b) & is.na(dat$v767c)) ~ NA,
                                     TRUE ~ FALSE)
} else {
  dat$sexpaid12m <- dplyr::case_when(dat$v791a == 1 |
                                       (dat$v767a == 6 | dat$v767b == 6 |
                                          dat$v767c == 6) ~ TRUE,
                                     (is.na(dat$v791a) & is.na(dat$v767a) & is.na(dat$v767b) &
                                        is.na(dat$v767c)) ~ NA,
                                     TRUE ~ FALSE)
}

# giftsvar = indicator for whether the survey includes any non-missing observations
# on question v791a (i.e. whether it was in the questionnaire)
dat <- dplyr::mutate(dat, giftsvar = ifelse(sum(!is.na(v791a))>0,1,0))

# # sti12m = whether the person reports having had an STI, genital sore/ulcer, or
# # genital discharge in the past 12 months (recode v763a-c)
# # Only set as NA if were a don't know/missing for all three of the questions
# dat$sti12m <- dplyr::case_when(dat$v763a == 1 | dat$v763b == 1 |
#                                   dat$v763c == 1 ~ TRUE,
#                                 dat$v763a %in% c(8,9) & dat$v763b %in% c(8,9) &
#                                   dat$v763c %in% c(8,9) ~ NA,
#                                 is.na(dat$v763a) & is.na(dat$v763b) &
#                                   is.na(dat$v763c) ~ NA,
#                                 TRUE ~ FALSE)

dat$SurveyId <- SurveyId

# Alterations to make the outcomes closer to being categorical
# As well as adding in a new variable, sexnonregplus, which is sexnonreg with all
# the individuals in sexpaid12m added on as well
dat <- dat %>%
  dplyr::select(SurveyId, individual_id, sex12m, nosex12m, sexcohab, sexcohabspouse, sexnonreg, sexpaid12m, giftsvar) %>%
  dplyr::mutate(
    # When nosex12m = 1, set sexpaid12m = 0
    # Being paid for sex in the last year requires having had sex in the past year
    sexpaid12m = ifelse(nosex12m == 1, 0, sexpaid12m),
    # # When sex12m = 1, set eversex = 1
    # # Having sex in the past year implies having ever had sex
    # eversex = ifelse(sex12m == 1, 1, eversex),
    # Set all rows with sexpaid12m = 1 to be in that category
    # This is questionable: being paid for sex doesn't imply not being in sexcohab or sexnonreg
    # But this is the assumption of the risk categories
    # eversex = ifelse(sexpaid12m == 1, 1, eversex),
    nosex12m = ifelse(sexpaid12m == 1, 0, nosex12m),
    sexcohabspouse = ifelse(sexpaid12m == 1, 0, sexcohabspouse),
    sexcohab = ifelse(sexpaid12m == 1, 0, sexcohab),
    sexnonreg = ifelse(sexpaid12m == 1, 0, sexnonreg),
    # Create new sexnonregplus variable
    sexnonregplus = ifelse(sexpaid12m == 1, 1, sexnonreg),
    # Turn everything from TRUE / FALSE coding to 1 / 0
    dplyr::across(sex12m:sexnonregplus, ~ as.numeric(.x))
  )

#' Checking the distributions of sexcohab and sexcohabspouse
summary(dat$sexcohab)
summary(dat$sexcohabspouse)
summary(dat$sexnonregplus)

#' Compare to the most recent PHIA
phia_survey_sexbehav <- read_csv("depends/mwi2016phia_survey_sexbehav.csv")
summary(phia_survey_sexbehav$sexcohab)
summary(phia_survey_sexbehav$sexnonregplus)

# orderly::orderly_develop_start("bwa_survey_bais")
# setwd("src/bwa_survey_bais")

sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")
url <- "sites/HIVInferenceGroup-WP/Shared Documents/Data/household surveys/Botswana/BAIS IV/bais-iv-2013-20150319-v1.dta"
path <- sharepoint$download(URLencode(url))

raw <- read_dta(path)

## District_Code                    "District Code(2)"
## EA_Code                          "EA number"
## DWELLING_NO                      NULL
## HHOLD_NO                         NULL
## UrbanRural                       "Type of locality"
## Stratum_No                       "Str. No."
## LocalityType                     "Locality Type"
## No_ofHholds                      "No. of Hholds"


## q607a                            "Have you ever been tested for HIV, the virus that causes AIDS"
## q607b                            "What was the main reason for testing"
## q607bf                           "specify other reasons"
## q607c                            "Why havent you tested"
## q608a                            "In the past 12 months how many times have you been tested?"
## q609                             "Where did you go for your last test?"
## q610a                            "Were you told/given your results for your last HIV test?"
## q610b                            "What was the result (answer if you don't mind)?"
## q611                             "Are you currently taking ARVs to treat your infection?"

## q301                             "Have you ever had sexual intercourse?"
## q307a                            "Have you had sex in the past 12 months?"
## q307b                            "In the last 12 months with how many people overall have you had sex?"
## q308_p1, q308_p2, q308_p3        "What is your relationship to [MOST RECENT/NEXT MOST RECENT PARTNER]"
## q319                             "In the last 12 months have you ever been paid or received gifts for sex?"
## q411                             "During the last 12 months, have you had any of the following symptoms?"

## Weight1                          "Weight at EA level"
## Age_reconciled                   "Age"
## Sex_reconciled                   "Sex of person"
## INCIDENCE_RESULTS                "BED RESULTS"
## PrimaryLast                      "Indicator of each last matching case as Primary"
## HIV_LAB_Result                   "Final HIV status as tested and confirmed in lab"
## FINAL_HIVResult                  "Final HIV status adjusted for ART"

#' The dataset has a variable `Stratum_No`, but it's not very clear how the are
#' The survey report contains the following information about the strata (page 15):
#"
#' > Stratification was therefore undertaken such that all districts and major
#' > urban centers became their own strata. To increase precision, consideration
#' > was also given to group EAs according to income categories in cities/towns
#' > and according to ecological zones in rural districts (implicit stratification).
#'
#' > Geographical stratification along ecological zones and income categories was
#' > expected to improve the accuracy of survey data in such a way that homogeneity
#' > of the variables was relatively high.
#' >
#' > There were five major rural ecological zones, namely:
#' > 1. Village
#' > 2. Lands
#' > 3. Cattle Post
#' > 4. Freehold Farms
#' > 5. Mixture of Land and Cattle Post
#' >
#' > During the delineation of maps, each EA was associated with the appropriate
#' > ecological zone. To facilitate the selection according to the stratification
#' > variables, rural EAs were listed in a specific order, for example, starting
#' > with cattle post, then farms, etc.
#'
#' It is somewhat unclear what this means. There are 26 districts in the dataset.
#' From investigating the datatset,
#'
#' * Strata 1-7 are Cities and Towns, and are also each a separate 'district' (District_Code 1-7)
#' * Strata 8 comprises the 'Urban Villages' component for all remaining districts
#' * Strata 9-27 are the 'Rural' component of each remaining district
#'   - There is no Stratum_No = 25
#'   - Ghanzi and Kgalagadi South are both coded as Stratum_No = 26
#'   - I suspect a coding error: Ghanzi intended to be Stratum_No = 25
#' * There are a few observations for Selebi-Phikwe, Chobe, and Ghanzi which do not have a
#'   Stratum_No. All are 'Rural' residence type. Assign based on above rules.

raw %>%
  count(as_factor(District_Code), Stratum_No) %>%
  spread(Stratum_No, n) %>%
  as.data.frame()

raw %>%
  count(UrbanRural, Stratum_No) %>%
  spread(Stratum_No, n) %>%
  print(n = Inf)

raw %>%
  filter(is.na(Stratum_No)) %>%
  count(District_Code, UrbanRural)

bais4 <- raw %>%
  mutate(
    district_code = as.integer(District_Code),
    district_name = as.character(as_factor(District_Code)),
    urban_rural = as.character(as_factor(UrbanRural)),
    stratum = case_when(
      district_name == "Ghanzi" & urban_rural == "Rural" ~ 25,
      is.na(Stratum_No) & district_name == "Selebi-Phikwe" ~ 4,
      is.na(Stratum_No) & district_name == "Chobe" & urban_rural == "Rural" ~ 24,
      is.na(Stratum_No) & district_name == "Ghanzi" & urban_rural == "Rural" ~ 25,
      TRUE ~ Stratum_No
    ),
  )

stopifnot(!is.na(bais4$stratum))
stopifnot(1:27 %in% bais4$stratum)

#' The same EA_Code appears in multiple strata and districts. It is unclear why.
#' Assign a new unique cluster_id

bais4 %>%
  count(district_code, district_name, stratum, urban_rural, ea_code = EA_Code) %>%
  group_by(ea_code) %>%
  filter(n() > 1) %>%
  arrange(ea_code) %>%
  print(n = Inf)

bais4 %>%
  select(district_code, district_name, stratum, urban_rural, EA_Code, DWELLING_NO, HHOLD_NO,
         GEOLOCATIONLatitude, GEOLOCATIONLongitude) %>%
  distinct() %>%
  tail()

#' There are 763 records with NA for DWELLING_NO and HHOLD_NO
#' * Spread across all districts, age, sex, and have HIV outcome data
#' * All have code "Other" for variable Completed = "Household questionnaire status"

bais4 %>% filter(is.na(DWELLING_NO)) %>% count(FINAL_HIVResult)
bais4 %>% filter(is.na(DWELLING_NO)) %>% count(AGE_RECONCILE_2) %>% print(n = Inf)

bais4 %>% filter(is.na(DWELLING_NO)) %>% count(district_name)
bais4 %>% filter(is.na(DWELLING_NO)) %>% count(stratum)

bais4 %>% filter(is.na(DWELLING_NO)) %>% count(Completed)

bais4 %>% filter(is.na(DWELLING_NO)) %>% count(IN_hhold)
bais4 %>% filter(is.na(DWELLING_NO)) %>% count(IN_individual)
bais4 %>% filter(is.na(DWELLING_NO)) %>% count(IN_blood)

bais4 %>% filter(is.na(DWELLING_NO)) %>% count(ID_RESPONDENT_LINE_NO)

#' ACTION:
#' * Assign a new cluster_id and individual_id
#' * Don't worry too much about household and dwelling number for now

clusters <- bais4 %>%
  select(district_code, district_name, stratum, urban_rural, EA_Code, DWELLING_NO, HHOLD_NO,
         GEOLOCATIONLatitude, GEOLOCATIONLongitude, RESULTS_VILLAGETOWN) %>%
  distinct() %>%
  group_by(district_code, district_name, stratum, urban_rural, EA_Code) %>%
  summarise(latitude = mean(GEOLOCATIONLatitude, na.rm = TRUE),
            longitude = mean(GEOLOCATIONLongitude, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(cluster_id = row_number())

#' All clusters have coordinates assigend except for EA_Code = 328.  The
#' following villages are mentioned in RESULTS_VILLAGETOWN for households
#' in this cluster: West Hanahai, East Hanahai, Xade, Ghanzi, and Dekar.
#'
#' Use coordinates for West Hanahai: -21.9886, 21.7657

clusters$latitude[clusters$EA_Code == 323] <- -21.9886
clusters$longitude[clusters$EA_Code == 323] <- 21.7657

stopifnot(!is.na(clusters$latitude))
stopifnot(!is.na(clusters$longitude))

bais4 <- bais4 %>%
  left_join(clusters,
            by = c("district_code", "district_name", "stratum", "urban_rural", "EA_Code"))

bais4 <- bais4 %>%
  mutate(individual_id = row_number())


#' ## Age and sex variable
#'
#' There's several age/sex variables in the dataset:
#'
#' * P03                              "Sex"
#' * P04_AGE_MONTHS                   "Age in months"
#' * P04_AGE_WEEKS                    "Age in weeks"
#' * P04_AGE_YEARS                    "How old is..... In completed years?"
#' * Q101                             "Sex of the respondent"
#' * Q102                             "How old are you in completed years?
#' * AGE_YEARS                        "Age in years on blood sample"
#' * AGE_MONTHS                       "Age in months on blood sample"
#' * SEX_lab                          "Sex on blood sample"
#' * Age_reconciled                   "Age"
#' * Sex_reconciled                   "Sex of person"
#' * AgeGroup                         "Age reconciled version2 (Binned)"
#' * AGE_RECONCILE_2                  "Age reconciled version2"
#' * Agegrp_positive                  "Age of positive person"
#' * Sex_tested                       "Sex of person tested"
#'
#' For age variable, comparison with survey report distribution in Figure 1 and
#' the variables AGE_RECONCILE_2 and AgeGroup confirm that AGE_RECONCILE_2
#' is the variable consistent with survey report.

count(bais4, AGE_RECONCILE_2, agegr = as_factor(AgeGroup), wt = Weight1) %>%
  spread(agegr, n) %>%
  print(n = Inf)

count(bais4, Age_reconciled, agegr = as_factor(AgeGroup), wt = Weight1) %>%
  spread(agegr, n) %>%
  print(n = Inf)

count(bais4, AgeGroup, wt = Weight1) %>%
  mutate(prop = n / sum(n))

#' Sex_reconciled, P03, and Sex_tested are consistent.
count(bais4, Sex_reconciled, Sex_tested)
count(bais4, Sex_reconciled, P03)

bais4 <- bais4 %>%
  mutate(
    sex = tolower(as.character(as_factor(raw$Sex_reconciled))),
    age = AGE_RECONCILE_2
  )


#' ## Code HIV result
#'
#' HIV result variables:
#'
#' * q610b                            "What was the result (answer if you don't mind)?"
#' * q611                             "Are you currently taking ARVs to treat your infection?"
#' * HIV_LAB_Result                   "Final HIV status as tested and confirmed in lab"
#' * FINAL_HIVResult                  "Final HIV status adjusted for ART"
#' * artynd                           "ART YND"
#'
#' The variable FINAL_HIVResult contains individuals who were 'Negative', 'intdeterminate'
#' and missing in the variable HIV_LAB_Result recoded ot HIV positive.

count(bais4, HIV_LAB_Result, FINAL_HIVResult)

#' Comparison with results in report indicate that FINAL_HIVResult is  consistent with
#' results in report.

#' The variable `artynd` was used to adjust HIV_LAB_Results to code FINAL_HIVResult. However,
#' unsure what the source for `artynd` was.

count(bais4, HIV_LAB_Result, artynd, FINAL_HIVResult)

#' `artynd` is correlated to q611, but not exactly the same and includes results for many
#' cases that are NA for q611

count(bais4, q611, artynd)

bais4 <- bais4 %>%
  mutate(
    hivstatus = as.integer(bais4$FINAL_HIVResult),
    evertest = as.integer(q607a == 1 & q610a == 1),  # tested and received result of last test
    test12m = as.integer(evertest & (!is.na(q608a) & q608a >= 1)),
    test12m = if_else(is.na(evertest), NA_integer_, test12m),
    artself = recode(as.integer(q611), `1` = 1L, `2` = 0L)
  )

#' ## Code sex behaviour
#'
#' Sex behaviour variables:
#'
#' * Q301                             "Have you ever had sexual intercourse?" YES 1, NO 2
#' * Q307A                            "Have you had sex in the past 12 months?" YES 1, NO 2
#' * Q307B                            "In the last 12 months with how many people overall have you had sex?" NUMBER
#' * Q308_P1, Q308_P2, Q308_P3        "What is your relationship to [MOST RECENT/NEXT MOST RECENT PARTNER]"
#' HUSBAND/WIFE 1, LIVING TOGETHER 2, GIRLFRIEND/BOYFRIEND 3, PAID 4, CASUAL 5
#' * Q108A                            "Does your husband/wife/partner live with you?" YES 1, NO 2
#' * Q319                             "In the last 12 months have you ever been paid or received gifts for sex?" YES 1, NO 2
#' * Q411                             "During the last 12 months, have you had any of the following symptoms?"
#' * Q411_2                           "GENITAL DISCHARGE"
#' * Q411_6                           "GENITAL ULCERS/OPEN SORES"
#'
#' For STI indicators, using genital discharge and ulcers/open sores to be equivalent to DHS questions - DHS
#' uses reporting having had an STI, having had a genital sore/ulcer, and having had genital discharge

cas_cats <- c(3, 4, 5, 6)

bais4 <- bais4 %>%
  mutate(
    eversex = recode(as.integer(Q301), `1` = TRUE, `2` = FALSE),
    sex12m = case_when(
      Q301 == 2 ~ FALSE,
      Q307A == 2 ~ FALSE,
      Q307A == 1 ~ TRUE,
      TRUE ~ NA
    ),
    nosex12m = 1 - sex12m,
    sexcohab = case_when(
      sex12m == FALSE ~ FALSE,
      Q307B == 1 & (Q308_P1 == 2 | Q308_P2 == 2 | Q308_P3 == 2) ~ TRUE,
      Q307B == 1 & (Q308_P1 == 1 | Q308_P2 == 1 | Q308_P3 == 1) & Q108A == 1 ~ TRUE,
      is.na(sex12m) ~ NA,
      TRUE ~ FALSE
    ),
    sexcohabspouse = case_when(
      sex12m == FALSE ~ FALSE,
      Q307B == 1 & ((!Q308_P1 %in% cas_cats) & (!Q308_P2 %in% cas_cats) & (!Q308_P3 %in% cas_cats)) ~ TRUE,
      is.na(sex12m) ~ NA,
      TRUE ~ FALSE
    ),
    sexnonreg = case_when(
      sex12m == FALSE ~ FALSE,
      Q108A == 2 ~ TRUE,
      Q308_P1 %in% cas_cats | Q308_P2 %in% cas_cats | Q308_P3 %in% cas_cats ~ TRUE,
      Q307B > 1 ~ TRUE,
      is.na(sex12m) ~ NA,
      TRUE ~ FALSE
    ),
    sexnonregspouse = case_when(
      sex12m == FALSE ~ FALSE,
      Q307B == 1 & (Q308_P1 == 1 | Q308_P2 == 1 | Q308_P3 == 1) & Q108A == 1 ~ FALSE,
      Q308_P1 %in% cas_cats | Q308_P2 %in% cas_cats | Q308_P3 %in% cas_cats ~ TRUE,
      Q307B > 1 ~ TRUE,
      is.na(sex12m) ~ NA,
      TRUE ~ FALSE
    ),
    sexpaid12m = case_when(
      Q319 == 1 | (Q308_P1 == 4 | Q308_P2 == 4 | Q308_P3 == 4) ~ TRUE,
      (is.na(Q319) & is.na(Q308_P1) & is.na(Q308_P2) & is.na(Q308_P3) & is.na(sex12m)) ~ NA,
      TRUE ~ FALSE
    ),
    sexnonregplus = ifelse(sexpaid12m == 1, 1, sexnonreg),
    sexnonregspouseplus = ifelse(sexpaid12m == 1, 1, sexnonregspouse),
    sti12m = case_when(
      Q411_2 == 1 | Q411_6 == 1 ~ TRUE,
      (is.na(Q411_2) & is.na(Q411_6) & is.na(sex12m)) ~ NA,
      TRUE ~ FALSE
    ),
    giftsvar = case_when(sum(!is.na(Q319)) > 0 ~ TRUE, TRUE ~ FALSE),
    # Just want the highest risk category that an individual belongs to
    nosex12m = ifelse(sexcohab | sexnonreg | sexpaid12m, FALSE, nosex12m),
    sexcohab = ifelse(sexnonreg | sexpaid12m, FALSE, sexcohab),
    sexcohabspouse = ifelse(sexnonregspouse | sexpaid12m, FALSE, sexcohabspouse),
    sexnonreg = ifelse(sexpaid12m, FALSE, sexnonreg),
    sexnonregspouse = ifelse(sexpaid12m, FALSE, sexnonregspouse),
    # Turn everything from TRUE / FALSE coding to 1 / 0
    across(sex12m:giftsvar, ~ as.integer(.x))
  )

bais4out <- bais4 %>%
  select(district_code, district_name, stratum, urban_rural, cluster_id, latitude, longitude,
         individual_id, sex, age, hivstatus, evertest, test12m, artself, Weight1,
         sex12m, nosex12m, sexcohab, sexcohabspouse, sexnonreg, sexnonregspouse,
         sexpaid12m, sexnonregplus, sexnonregspouseplus, giftsvar)

write_csv(bais4out, "bwa2013bais-recode-sexbehav.csv", na = "")

###### TO FIX:
###### define at top of the file the quarter you want to filter the Naomi results
###### to - e.g. iso3 == "ZAF" ~ calendar_quarter == "CY2022Q3" needs to be fixed
###### multiple times within the script right now

#' Countries
priority_iso3 <- multi.utils::priority_iso3()
analysis_level <- multi.utils::analysis_level()

#####################################################################
## Read in Naomi files + clean up so that they can all be appended
#####################################################################

# come up with more sustainable solution to read in this file - file is not on
# Imperial Sharepoint and can't read in directly from UNAIDS sharepoint
# naomi_output <- readRDS("~/Downloads/naomi1.rds")
naomi_output <- readRDS("~/Downloads/naomi1_2023_07_05.rds")

# naomi_mwi <- read_csv("~/Downloads/indicators_mwi.csv")
# naomi_mwi$iso3 <- "MWI"
# naomi_mwi$country <- "Malawi"
# naomi_mwi$region <- "ESA"
#
# naomi_moz <- read_csv("~/Downloads/PRELIMINARY NAOMI indicators_13.04.2023.csv")
# naomi_moz$iso3 <- "MOZ"
# naomi_moz$country <- "Mozambique"
# naomi_moz$region <- "ESA"
#
# naomi_zaf <- read_csv("~/Downloads/zaf_naomi-output_dhis-coarse_thembisa-calibrated-fine_2023-03-20/indicators.csv")
# naomi_zaf$iso3 <- "ZAF"
# naomi_zaf$country <- "South Africa"
# naomi_zaf$region <- "ESA"
#
# naomi_caf <- read_csv("~/Downloads/CAF naomi_outputs/indicators.csv")
# naomi_caf$iso3 <- "CAF"
# naomi_caf$country <- "Central African Republic"
# naomi_caf$region <- "WCA"
#
# # process this year's AGO data - missing a huge amount of columns
# naomi_ago <- read_csv("~/Downloads/ago_plhiv_spectrum2022_totals_dissagreagted_by_ago-dhs2015_sae.csv")
# ago_areas <- read_sf("~/Downloads/ago_areas.geojson") %>% st_drop_geometry()
# # need to create 15-24 and 15-49 year age categories
# naomi_ago <- naomi_ago %>%
#   bind_rows(
#     naomi_ago %>%
#       filter(age_group %in% c("Y015_019","Y020_024")) %>%
#       group_by(area_id,sex,indicator,calendar_quarter,source) %>%
#       summarise(value = sum(value),
#                 age_group = "Y015_024")
#   ) %>%
#   bind_rows(
#     naomi_ago %>%
#       filter(age_group %in% c("Y015_019","Y020_024","Y025_029","Y030_034",
#                               "Y035_039","Y040_044","Y045_049")) %>%
#       group_by(area_id,sex,indicator,calendar_quarter,source) %>%
#       summarise(value = sum(value),
#                 age_group = "Y015_049")
#   )
#
# # Also need to create level 1 and 0 data
# naomi_ago <- naomi_ago %>%
#   bind_rows(
#     naomi_ago %>%
#   left_join(
#     ago_areas %>%
#       st_drop_geometry() %>%
#       filter(area_level==2) %>%
#       select(area_id,parent_area_id),
#     by = "area_id"
#   ) %>%
#   group_by(parent_area_id,sex,age_group,indicator,calendar_quarter,source) %>%
#   summarise(value = sum(value)) %>%
#   rename(area_id = parent_area_id)
#   )
# naomi_ago <- naomi_ago %>%
#   bind_rows(
#     naomi_ago %>%
#       left_join(
#         ago_areas %>%
#           st_drop_geometry() %>%
#           filter(area_level==1) %>%
#           select(area_id,parent_area_id),
#         by = "area_id"
#       ) %>%
#       group_by(parent_area_id,sex,age_group,indicator,calendar_quarter,source) %>%
#       summarise(value = sum(value)) %>%
#       rename(area_id = parent_area_id)
#   )
#
# naomi_ago <- naomi_ago %>%
#   bind_rows(
#     naomi_ago %>%
#       group_by(area_id,sex,age_group,calendar_quarter,source) %>%
#       mutate(prevalence = value[indicator=="plhiv"] / value[indicator=="population"]) %>%
#       select(-indicator,-value) %>%
#       rename(value = prevalence) %>%
#       mutate(indicator = "prevalence") %>%
#       distinct()
#   ) %>%
#   bind_rows(
#     naomi_ago %>%
#       group_by(area_id,sex,age_group,calendar_quarter,source) %>%
#       mutate(incidence = value[indicator=="infections"] / (value[indicator=="population"] - value[indicator=="plhiv"])) %>%
#       select(-indicator,-value) %>%
#       rename(value = incidence) %>%
#       mutate(indicator = "incidence") %>%
#       distinct()
#   )
#
# naomi_ago$iso3 <- "AGO"
# naomi_ago$country <- "Angola"
# naomi_ago <- naomi_ago %>%
#   left_join(select(ago_areas,
#                    area_id,area_name,area_level,area_level_label))
# naomi_ago <- naomi_ago %>%
#   left_join(naomi_output %>%
#               select(age_group,age_group_label) %>%
#               distinct())
# naomi_ago$quarter_label <- "December 2022"
# naomi_ago$mean <- naomi_ago$value # not sure which this is and whether we use the
#   # mean or median but assuming doesn't really matter since this is a quite shady
#   # way to produce estimates anyways.
# naomi_ago$median <- naomi_ago$value
# naomi_ago$se <- NA_complex_
# naomi_ago$mode <- NA_complex_
# naomi_ago$lower <- NA_complex_
# naomi_ago$upper <- NA_complex_
# naomi_ago$region <- "ESA"
# naomi_ago <- naomi_ago %>%
#   left_join(naomi_output %>%
#               filter(country=="South Africa") %>%
#               select(indicator,indicator_label) %>%
#               distinct())

# naomi_output <- naomi_output %>%
#   filter(iso3!="MWI" & iso3!="MOZ" & iso3!="ZAF" & iso3!="CAF" & iso3!="AGO") %>%
#   bind_rows(list(naomi_mwi,naomi_moz, naomi_zaf, naomi_caf, naomi_ago))



#####################################################################
## Clean up Naomi to pull into later estimation steps
#####################################################################


naomi_extract <- naomi_output %>%
  filter(
    iso3 %in% priority_iso3,
    indicator %in% c("population", "plhiv", "infections"),
    #' These are the age groups we are considering, plus those which are useful for disaggregation
    age_group_label %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "15-24","25-49", "15-49"),
    #' female & male, not both
    sex %in% c("female","male"),
    #' Value being used in the global report (there is no CY2021Q4 in ZAF)
    case_when(
      iso3 == "ZAF" ~ calendar_quarter == "CY2022Q3",
      iso3 == "MOZ" ~ calendar_quarter == "CY2022Q4",
      iso3 == "CAF" ~ calendar_quarter == "CY2022Q4",
      iso3 == "AGO" ~ calendar_quarter == "CY2022Q4",
      TRUE ~ calendar_quarter == "CY2022Q4"
    )
  ) %>%
  left_join(
    as.data.frame(analysis_level) %>%
      tibble::rownames_to_column("iso3"),
    by = "iso3"
  ) %>%
  filter(area_level <= analysis_level) %>%
  select(-analysis_level)

saveRDS(naomi_extract, "src/process_naomi-data/naomi_extract.rds")
saveRDS(naomi_extract, "src/process_naomi-data_men/naomi_extract.rds")


#####################################################################
## Run task to get Naomi data ready for incorporation into other tasks
#####################################################################

run_commit_push("process_naomi-data")
run_commit_push("process_naomi-data_men")

#####################################################################
## Clean up Naomi to pull into the Excel spreadsheet
#####################################################################

extra_analysis_level <- c(analysis_level,c("BEN" = 2,
                                           "GMB" = 2, "GNB" = 1, "GNQ" = 2,
                                           "NGA" = 2, "SEN" = 2, "STP" = 1))
#done stupidly!
naomi_extract <- naomi_output %>%
  filter(
    indicator %in% c("population", "plhiv", "infections","incidence"),
    #' These are the age groups we are considering, plus those which are useful for disaggregation
    age_group_label %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "15-24", "25-49", "15-49"),
    #' Value being used in the global report (there is no CY2021Q4 in ZAF)
    case_when(
      iso3 == "ZAF" ~ calendar_quarter == "CY2022Q3",
      iso3 == "MOZ" ~ calendar_quarter == "CY2022Q4",
      iso3 == "CAF" ~ calendar_quarter == "CY2022Q4",
      iso3 == "AGO" ~ calendar_quarter == "CY2022Q4",
      TRUE ~ calendar_quarter == "CY2022Q4"
    )
  ) %>%
  left_join(
    data.frame(extra_analysis_level = extra_analysis_level, iso3=names(extra_analysis_level)),
    by = "iso3"
  ) %>%
  filter(area_level==extra_analysis_level) %>%
  select(-extra_analysis_level) %>%
  mutate(mean = as.character(ifelse(indicator=="incidence",mean*100,mean))) %>%
  bind_rows(
    naomi_output %>%
      filter(
        indicator %in% c("population", "plhiv", "infections","incidence"),
        #' These are the age groups we are considering, plus those which are useful for disaggregation
        age_group_label %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "15-24", "25-49", "15-49"),
        #' Value being used in the global report (there is no CY2021Q4 in ZAF)
        case_when(
          iso3 == "ZAF" ~ calendar_quarter == "CY2022Q3",
          iso3 == "MOZ" ~ calendar_quarter == "CY2022Q4",
          iso3 == "CAF" ~ calendar_quarter == "CY2022Q4",
          iso3 == "AGO" ~ calendar_quarter == "CY2022Q4",
          TRUE ~ calendar_quarter == "CY2022Q4"
        )
      ) %>%
      left_join(
        data.frame(extra_analysis_level = extra_analysis_level, iso3=names(extra_analysis_level)),
        by = "iso3"
      ) %>%
      filter(area_level==extra_analysis_level) %>%
      select(-extra_analysis_level) %>%
      filter(indicator=="incidence") %>%
      mutate(Incicategory = case_when(mean<0.003 ~ "Low",
                                      mean>=0.003 & mean<0.01 ~ "Moderate",
                                      mean>=0.01 & mean<0.03 ~ "High",
                                      mean>=0.03 ~ "Very High",
                                      TRUE ~ NA_character_)) %>%
      select(-mean) %>%
      mutate(mean = Incicategory,
             indicator = "Incicategory") %>%
      select(-Incicategory)
  ) %>%
  mutate(indicator = recode(indicator, "population" = "Pop", "plhiv" = "PLHIV", "infections" = "new",
                            "incidence" = "Inci"),
         sex = recode(sex, "female" = "f", "male" = "m", "both" = "all")) %>%
  select(iso3,area_id,area_name,indicator,age_group_label,sex,mean) %>%
  pivot_wider(id_cols = c(iso3,area_id,area_name),
              names_from = c(indicator,age_group_label,sex),
              names_sep = "",
              values_from = mean) %>%
  mutate(Country = fct_recode(iso3,
                              "Botswana" = "BWA",
                              "Cameroon" = "CMR",
                              "Kenya" = "KEN",
                              "Lesotho" = "LSO",
                              "Mozambique" = "MOZ",
                              "Malawi" = "MWI",
                              "Namibia" = "NAM",
                              "Eswatini" = "SWZ",
                              "Tanzania" = "TZA",
                              "Uganda" = "UGA",
                              "South Africa" = "ZAF",
                              "Zambia" = "ZMB",
                              "Zimbabwe" = "ZWE",
                              "Angola" = "AGO",
                              "Burundi" = "BDI",
                              "Congo Democratic Republic" = "COD",
                              "Ethiopia" = "ETH",
                              "Gabon" = "GAB",
                              "Haiti" = "HTI",
                              "Rwanda" = "RWA",
                              "Chad" = "TCD",
                              "Burkina Faso" = "BFA",
                              "Cote d'Ivoire" = "CIV",
                              "Ghana" = "GHA",
                              "Guinea" = "GIN",
                              "Liberia" = "LBR",
                              "Mali" = "MLI",
                              "Niger" = "NER",
                              "Sierra Leone" = "SLE",
                              "Togo" = "TGO",
                              "Benin" = "BEN",
                              "Central African Republic" = "CAF",
                              "Congo" = "COG",
                              "The Gambia" = "GMB",
                              "Equitorial guinea" = "GNQ",
                              "Nigeria" = "NGA",
                              "Senegal" = "SEN",
                              "Sao Tome and Principe" = "STP"
  )) %>%
  left_join(
    naomi_output %>%
      filter(age_group_label=="all ages",
             sex=="both",
             indicator=="infections",
             case_when(
               iso3 == "ZAF" ~ calendar_quarter == "CY2022Q3",
               iso3 == "MOZ" ~ calendar_quarter == "CY2022Q4",
               iso3 == "CAF" ~ calendar_quarter == "CY2022Q4",
               iso3 == "AGO" ~ calendar_quarter == "CY2022Q4",
               TRUE ~ calendar_quarter == "CY2022Q4"
             )) %>%
      select(area_id,mean)
  ) %>%
  rename("newAll" = "mean") %>%
  select(Country,area_id,area_name,`Pop15-24all`,`Pop15-24f`,`Pop15-24m`,
         `PLHIV15-24all`,`PLHIV15-24f`,`PLHIV15-24m`,
         newAll, `new15-24all`,`new15-24f`,`new15-24m`,
         `Inci15-24f`,`Incicategory15-24f`,`Inci15-24m`,`Incicategory15-24m`,
         `Pop15-19all`,`Pop15-19f`,`Pop15-19m`,
         `PLHIV15-19all`,`PLHIV15-19f`,`PLHIV15-19m`,
         `new15-19all`,`new15-19f`,`new15-19m`,
         `Inci15-19f`,`Incicategory15-19f`,`Inci15-19m`,`Incicategory15-19m`,
         `Pop20-24all`,`Pop20-24f`,`Pop20-24m`,
         `PLHIV20-24all`,`PLHIV20-24f`,`PLHIV20-24m`,
         `new20-24all`,`new20-24f`,`new20-24m`,
         `Inci20-24f`,`Incicategory20-24f`,`Inci20-24m`,`Incicategory20-24m`,
         `Pop25-49all`,`Pop25-49f`,`Pop25-49m`,
         `PLHIV25-49all`,`PLHIV25-49f`,`PLHIV25-49m`,
         `new25-49all`,`new25-49f`,`new25-49m`,
         `Inci25-49f`,`Incicategory25-49f`,`Inci25-49m`,`Incicategory25-49m`,
         `Pop25-29all`,`Pop25-29f`,`Pop25-29m`,
         `PLHIV25-29all`,`PLHIV25-29f`,`PLHIV25-29m`,
         `new25-29all`,`new25-29f`,`new25-29m`,
         `Inci25-29f`,`Incicategory25-29f`,`Inci25-29m`,`Incicategory25-29m`,
         `Pop30-34all`,`Pop30-34f`,`Pop30-34m`,
         `PLHIV30-34all`,`PLHIV30-34f`,`PLHIV30-34m`,
         `new30-34all`,`new30-34f`,`new30-34m`,
         `Inci30-34f`,`Incicategory30-34f`,`Inci30-34m`,`Incicategory30-34m`,
         `Pop35-39all`,`Pop35-39f`,`Pop35-39m`,
         `PLHIV35-39all`,`PLHIV35-39f`,`PLHIV35-39m`,
         `new35-39all`,`new35-39f`,`new35-39m`,
         `Inci35-39f`,`Incicategory35-39f`,`Inci35-39m`,`Incicategory35-39m`,
         `Pop40-44all`,`Pop40-44f`,`Pop40-44m`,
         `PLHIV40-44all`,`PLHIV40-44f`,`PLHIV40-44m`,
         `new40-44all`,`new40-44f`,`new40-44m`,
         `Inci40-44f`,`Incicategory40-44f`,`Inci40-44m`,`Incicategory40-44m`,
         `Pop45-49all`,`Pop45-49f`,`Pop45-49m`,
         `PLHIV45-49all`,`PLHIV45-49f`,`PLHIV45-49m`,
         `new45-49all`,`new45-49f`,`new45-49m`,
         `Inci45-49f`,`Incicategory45-49f`,`Inci45-49m`,`Incicategory45-49m`,
         `Pop15-49all`,`Pop15-49f`,`Pop15-49m`,
         `PLHIV15-49all`,`PLHIV15-49f`,`PLHIV15-49m`,
         `new15-49all`,`new15-49f`,`new15-49m`,
         `Inci15-49f`,`Incicategory15-49f`,`Inci15-49m`,`Incicategory15-49m`)

## Clean up area names and Country names
naomi_extract$area_name[naomi_extract$Country=="Angola"] <- stringr::str_to_title(naomi_extract$area_name[naomi_extract$Country=="Angola"])

naomi_extract$Country[naomi_extract$Country=="GHANA"] <- "Ghana"
naomi_extract$Country[naomi_extract$Country=="SAO TOME AND PRINCIPE"] <- "Sao Tome and Principe"

naomi_extract <- naomi_extract %>%
  mutate(Country = fct_recode(Country,
                              `Guinea Bissau` = "GNB",
                              `Equatorial Guinea` = "Equitorial guinea",
                              `Democratic Republic of the Congo` = "Congo Democratic Republic"))

guinea_chad_dat <- naomi_extract %>% filter(Country=="Guinea" | Country=="Chad") %>%
  select(area_id,area_name)

guinea_chad_dat$area_name_fixed <- iconv(guinea_chad_dat$area_name,from="UTF-8",to="LATIN1")

naomi_extract <- naomi_extract %>%
  left_join(select(guinea_chad_dat,area_id,area_name_fixed),by="area_id") %>%
  mutate(area_name = ifelse(!is.na(area_name_fixed),area_name_fixed,area_name)) %>%
  select(-area_name_fixed)

writexl::write_xlsx(naomi_extract,"src/process_naomi-data/naomi_extract.xlsx")




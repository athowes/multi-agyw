#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("process_katies-spreadsheet")
# setwd("src/process_katies-spreadsheet")

df <- read_csv("depends/best-3-multinomial-smoothed-district-sexbehav.csv")
areas <- readRDS("depends/areas.rds")

#' Confusing file names here, but I believe they are all the same thing
files <- c(
  "zwe_20210331-161516_naomi_spectrum_digest",
  "zaf_district_naomi-output_calibrated-province",
  "ken-naomi-output_calibrated-sex-age-coarse_2021-03-30",
  "CMR_20210427-155423_naomi_spectrum_digest",
  "MOZ_20210504-101529_naomi_spectrum_digest",
  "LSO_naomi-output_20210616-0905",
  "MWI_20210428-213633_sex-age-coarse-calibrated_naomi_spectrum_digest",
  "zmb-naomi-output_calibrated-sex-age-coarse_2021-04-27",
  "SWZ_naomi-output_20210616-1231",
  "nam_20210408-003934_naomi_spectrum_digest",
  "uga-2016-art-adjusted_naomi-output_calibrated-sex-age-fine_2021-02-05",
  "bwa_20210329-142306_naomi_spectrum_digest",
  "tza-naomi-output-time2-art_calibrated-sex-age-coarse_2021-04-13"
)

naomi <- lapply(files, function(file) read.csv(paste0("naomi-output/", file, "/indicators.csv"))) %>%
  bind_rows()

naomi %>%
  filter(
    age_group %in% c("Y015_019", "Y020_024", "Y025_029", "Y015_024"),
    indicator %in% c("infections", "plhiv", "population"),
    sex %in% c("female", "both"),
    (
      (calendar_quarter == "CY2019Q4") |
      (calendar_quarter=="CY2019Q3" & substr(area_id, 1, 3) == "ZAF") |
      (calendar_quarter == "CY2020Q4" & substr(area_id,1,3) == "CMR")
    )
  ) %>%
  select(area_id, sex, age_group, indicator, mean) %>%
  pivot_wider(
    names_from = indicator,
    values_from = mean
  ) %>%
  mutate(
    incidence = 100 * infections / (population - plhiv),
    incidence_cat = cut(
      incidence,
      c(0, 0.1, 0.3, 1, 10^6),
      labels = c("Low", "Moderate", "High", "Very High"),
      include.lowest = TRUE,
      right = TRUE
    )
  )

#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("process_zaf")
# setwd("src/process_zaf")

women_dat <- read_csv("depends/incidence-district-sexbehav_allages_women.csv")

women_dat <- women_dat %>%
  filter(iso3=="ZAF")

men_dat <- read_csv("depends/incidence-district-sexbehav_allages_men.csv")

men_dat <- men_dat %>%
  filter(iso3=="ZAF")

# naomi <- read_xlsx("naomi_extract.xlsx")
# naomi_zaf <- naomi %>%
#   filter(Country == "South Africa")

write_csv(women_dat,"incidence-district-sexbehav_allages_women_zaf.csv")
write_csv(men_dat,"incidence-district-sexbehav_allages_men_zaf.csv")
# write_xlsx(naomi_zaf,"naomi_zaf_extract.xlsx")

#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("process_caf")
# setwd("src/process_caf")

women_dat <- read_csv("depends/incidence-district-sexbehav_allages_women.csv")

women_dat <- women_dat %>%
  filter(iso3=="CAF")

men_dat <- read_csv("depends/incidence-district-sexbehav_allages_men.csv")

men_dat <- men_dat %>%
  filter(iso3=="CAF")

naomi <- read_xlsx("naomi_extract.xlsx")
naomi_moz <- naomi %>%
  filter(Country == "Central African Republic")

write_csv(women_dat,"incidence-district-sexbehav_allages_women_caf.csv")
write_csv(men_dat,"incidence-district-sexbehav_allages_men_caf.csv")
write_xlsx(naomi_moz,"naomi_caf_extract.xlsx")

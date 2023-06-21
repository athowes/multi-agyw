#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("process_moz")
# setwd("src/process_moz")

women_dat <- read_csv("depends/incidence-district-sexbehav_allages_women.csv")

women_dat <- women_dat %>%
  filter(iso3=="MOZ")

men_dat <- read_csv("depends/incidence-district-sexbehav_allages_men.csv")

men_dat <- men_dat %>%
  filter(iso3=="MOZ")

naomi <- read_xlsx("naomi_extract.xlsx")
naomi_moz <- naomi %>%
  filter(Country == "Mozambique")

write_csv(women_dat,"incidence-district-sexbehav_allages_women_moz.csv")
write_csv(men_dat,"incidence-district-sexbehav_allages_men_moz.csv")
write_xlsx(naomi_moz,"naomi_moz_extract.xlsx")

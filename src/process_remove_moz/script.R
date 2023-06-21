#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("process_nomoz")
# setwd("src/process_nomoz")

women_dat <- read_csv("depends/incidence-district-sexbehav_allages_women.csv")

women_dat <- women_dat %>%
  filter(iso3!="MOZ")

men_dat <- read_csv("depends/incidence-district-sexbehav_allages_men.csv")

men_dat <- men_dat %>%
  filter(iso3!="MOZ")

naomi <- read_xlsx("naomi_extract.xlsx")
naomi_zaf <- naomi %>%
  filter(Country != "Mozambique")

write_csv(women_dat,"incidence-district-sexbehav_allages_women_nomoz.csv")
write_csv(men_dat,"incidence-district-sexbehav_allages_men_nomoz.csv")
write_xlsx(naomi_zaf,"naomi_nomoz_extract.xlsx")

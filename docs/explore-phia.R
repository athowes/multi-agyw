library(readr)
library(tidyverse)

phia_sexbehav <- read_csv("global/phia_sexbehav.csv")

#' Relevant PHIA surveys are:
#' LSO2016PHIA, MWI2015PHIA, NAM2017PHIA, SWZ2016PHIA, TZA2016PHIA, UGA2016PHIA, ZMB2016PHIA, CMR2017PHIA
phia_sexbehav %>%
  filter(
    substr(survey_id, 1, 3) %in% c("BWA", "CMR", "KEN", "LSO", "MOZ", "MWI", "NAM", "SWZ", "TZA", "UGA", "ZAF", "ZMB")
  ) %>%
  select(survey_id) %>%
  unique()

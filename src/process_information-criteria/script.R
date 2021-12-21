#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("process_information-criteria")
# setwd("src/process_information-criteria")

cbpalette <- c("#56B4E9","#009E73", "#E69F00", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")

available_surveys <- read_csv("depends/available-surveys.csv")

#' Four categories

#' All surveys with a transactional sex question
giftsvar_surveys <- available_surveys %>%
  filter(giftsvar == 1)

files <- paste0("depends/", tolower(unique(giftsvar_surveys$iso3)), "_4-information-criteria.csv")
df_4 <- bind_rows(lapply(files, function(file) read_csv(file))) %>%
  update_naming()

#' Countries with only one survey with a transactional sex question
iso3_4_single <- giftsvar_surveys %>%
  group_by(iso3) %>%
  summarise(count = length(unique(survey_id))) %>%
  filter(count == 1) %>%
  pull(iso3)

#' Countries with multiple surveys with a transactional sex question
iso3_4_multi <- giftsvar_surveys %>%
  group_by(iso3) %>%
  summarise(count = length(unique(survey_id))) %>%
  filter(count > 1) %>%
  pull(iso3)

#' Check that all the countries are covered
stopifnot(
  length(iso3_4_single) + length(iso3_4_multi) == length(unique(giftsvar_surveys$iso3))
)

#' Four category

#' Currently all the four category models are just spatial, no temporal

write_csv(df_4, "4-model-comparison.csv", na = "")

pdf("4-model-comparison.pdf", h = 5, w = 6.25)

ic_plot(df_4, ic = "dic")
ic_plot(df_4, ic = "waic")
ic_plot(df_4, ic = "cpo")
ic_plot(df_4, ic = "pit")

dev.off()

pdf("4-rank-comparison.pdf", h = 3.5, w = 6.25)

rank_ic_plot(df_4)

dev.off()

create_latex_table(df_4, file_name = "4-model-comparison.txt")

#' Three categories

files <- paste0("depends/", tolower(unique(available_surveys$iso3)), "_3-information-criteria.csv")
df_3 <- bind_rows(lapply(files, function(file) read_csv(file))) %>%
  update_naming()

#' Countries with only one survey
iso3_3_single <- available_surveys %>%
  group_by(iso3) %>%
  summarise(count = length(unique(survey_id))) %>%
  filter(count == 1) %>%
  pull(iso3)

#' Countries with multiple surveys
iso3_3_multi <- available_surveys %>%
  group_by(iso3) %>%
  summarise(count = length(unique(survey_id))) %>%
  filter(count > 1) %>%
  pull(iso3)

#' Single survey and three categories

df_3_single <- filter(df_3, iso3 %in% iso3_3_single)

write_csv(df_3_single, "3-single-model-comparison.csv", na = "")

pdf("3-single-model-comparison.pdf", h = 5, w = 6.25)

ic_plot(df_3_single, ic = "dic")
ic_plot(df_3_single, ic = "waic")
ic_plot(df_3_single, ic = "cpo")
ic_plot(df_3_single, ic = "pit")

dev.off()

pdf("3-single-rank-comparison.pdf", h = 3.5, w = 6.25)

rank_ic_plot(df_3_single)

dev.off()

create_latex_table(df_3, file_name = "3-single-model-comparison.txt")

#' Multiple surveys and three categories

df_3_multi <- filter(df_3, iso3 %in% iso3_3_multi)

write_csv(df_3_multi, "3-multi-model-comparison.csv", na = "")

pdf("3-multi-model-comparison.pdf", h = 5, w = 6.25)

ic_plot(df_3_multi, ic = "dic")
ic_plot(df_3_multi, ic = "waic")
ic_plot(df_3_multi, ic = "cpo")
ic_plot(df_3_multi, ic = "pit")

dev.off()

pdf("3-multi-rank-comparison.pdf", h = 3.5, w = 6.25)

rank_ic_plot(df_3_multi)

dev.off()

create_latex_table(df_3_multi, file_name = "3-multi-model-comparison.txt")

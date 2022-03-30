#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("process_information-criteria")
# setwd("src/process_information-criteria")

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

dev.off()

pdf("4-rank-comparison.pdf", h = 2.5, w = 6.25)

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

dev.off()

pdf("3-single-rank-comparison.pdf", h = 2.5, w = 6.25)

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

dev.off()

pdf("3-multi-rank-comparison.pdf", h = 2.5, w = 6.25)

rank_ic_plot(df_3_multi)

dev.off()

create_latex_table(df_3_multi, file_name = "3-multi-model-comparison.txt")

#' Together plots
pdf("3-rank-comparison.pdf", h = 5, w = 6.25)

cowplot::plot_grid(
  rank_ic_plot(df_3_single) +
    labs(title = "Countries with a single survey", y = "") +
    coord_flip(),
  rank_ic_plot(df_3_multi) +
    labs(title = "Countries with multiple surveys") +
    guides(fill = "none") +
    coord_flip(),
  ncol = 1,
  rel_heights = c(0.8, 1)
)

dev.off()

#' Logistic regression model
df_prop <- read_csv("depends/fsw-logit-information-criteria.csv")

df_prop %>%
  mutate(
    dic = paste0(dic, " (", dic_se, ")"),
    waic = paste0(waic, " (", waic_se, ")"),
    cpo = paste0(cpo, " (", cpo_se, ")"),
  ) %>%
  select(-contains("se")) %>%
  rename_with(~toupper(.), .cols = any_of(c("dic", "waic", "cpo"))) %>%
  rename_with(~str_to_title(.), .cols = any_of(c("model", "country"))) %>%
  select(-any_of(c("iso3"))) %>%
  pivot_longer(
    cols = c("DIC", "WAIC", "CPO"),
    names_to = "Criteria"
  ) %>%
  pivot_wider(
    names_from = "Model",
    values_from = "value"
  ) %>%
  gt() %>%
  fmt_missing(columns = everything(), rows = everything(), missing_text = "-") %>%
  #' It's clear from context that these are the criteria
  #' (such that the label is not required)
  cols_label(
    Criteria = "",
  ) %>%
  tab_stubhead(label = "") %>%
  as_latex() %>%
  as.character() %>%
  cat(file = "fsw-logit-model-comparison.txt")

#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("process_information-criteria")
# setwd("src/process_information-criteria")

available_surveys <- read_csv("depends/available-surveys.csv")

#' Fitting countries jointly

#' Multinomial model

df <- read_csv("depends/information-criteria.csv") %>%
  #' Round these to the nearest integer
  mutate_at(2:7, round, 0)

pdf("model-comparison.pdf", h = 3, w = 6.25)

df %>%
  rename("dic_mean" = "dic", "waic_mean" = "waic", "cpo_mean" = "cpo") %>%
  pivot_longer(
    cols = starts_with(c("dic", "waic", "cpo")),
    names_to = "name",
    values_to = "value"
  ) %>%
  separate(name, into = c("metric", "type"), extra = "merge", fill = "left") %>%
  pivot_wider(
    names_from = "type",
    values_from = "value"
  ) %>%
  mutate(
    metric = fct_recode(metric,
                        "DIC" = "dic",
                        "WAIC" = "waic",
                        "CPO" = "cpo"
    ),
    model = fct_recode(model,
                       "M1: IID spatial, IID temporal" = "Model 1",
                       "M1x: IID spatial, IID temporal, with interaction" = "Model 1x",
                       "M2: Besag spatial, IID temporal" = "Model 2",
                       "M2x: IID spatial, IID temporal, with interaction" = "Model 2x",
                       "M3: IID spatial, AR1 temporal" = "Model 3",
                       "M3x: IID spatial, IID temporal, with interaction" = "Model 3x",
                       "M4: Besag spatial, AR1 temporal" = "Model 4",
                       "M4x: IID spatial, IID temporal, with interaction" = "Model 4x",
    )
  ) %>%
  split(.$metric) %>%
  lapply(function(x)
    x %>%
      mutate(
        min_idx = (mean == min(mean, na.rm = TRUE)),
        max_idx = (mean == max(mean, na.rm = TRUE)),
        best_idx = ifelse(metric %in% c("WAIC", "DIC"), min_idx, max_idx)
      )
  ) %>%
  bind_rows() %>%
  ggplot(aes(x = model, y = mean, col = model, shape = best_idx)) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = mean - se, ymax = mean + se),
    stat = "identity", position = "dodge", alpha = 0.4, col = "black", width = 0
  ) +
  facet_wrap(~metric, scales = "free") +
  scale_color_manual(values = multi.utils::cbpalette()) +
  scale_shape_manual(values = c(16, 15)) +
  guides(shape = "none") +
  labs(y = "Value", x = "", col = "") +
  guides(col = guide_legend(nrow = 2, byrow = TRUE)) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 9),
    legend.margin = margin(0, 0, 0, 0)
  )

dev.off()

df %>%
  mutate(
    dic = paste0(dic, " (", dic_se, ")"),
    waic = paste0(waic, " (", waic_se, ")"),
    cpo = paste0(cpo, " (", cpo_se, ")"),
  ) %>%
  select(-contains("se")) %>%
  mutate(
    model = fct_recode(model,
                       "M1" = "Model 1", "M2" = "Model 2",
                       "M3" = "Model 3", "M4" = "Model 4")
  ) %>%
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
  cat(file = "model-comparison.txt")

#' Logistic model

df_prop <- read_csv("depends/fsw-logit-information-criteria.csv")

pdf("fsw-logit-model-comparison.pdf", h = 3, w = 6.25)

df_prop %>%
  rename("dic_mean" = "dic", "waic_mean" = "waic", "cpo_mean" = "cpo") %>%
  pivot_longer(
    cols = starts_with(c("dic", "waic", "cpo")),
    names_to = "name",
    values_to = "value"
  ) %>%
  separate(name, into = c("metric", "type"), extra = "merge", fill = "left") %>%
  pivot_wider(
    names_from = "type",
    values_from = "value"
  ) %>%
  mutate(
    metric = fct_recode(metric,
                        "DIC" = "dic",
                        "WAIC" = "waic",
                        "CPO" = "cpo"
    ),
    model = fct_recode(model,
                       "L1: IID spatial" = "Model 1",
                       "L2: Besag spatial" = "Model 2",
                       "L3: IID spatial, cfswever" = "Model 3",
                       "L4: Besag spatial, cfswever" = "Model 4",
                       "L5: IID spatial, cfswrecent" = "Model 5",
                       "L6: Besag spatial, cfswrecent" = "Model 6",
    )
  ) %>%
  split(.$metric) %>%
  lapply(function(x)
    x %>%
      mutate(
        min_idx = (mean == min(mean, na.rm = TRUE)),
        max_idx = (mean == max(mean, na.rm = TRUE)),
        best_idx = ifelse(metric %in% c("WAIC", "DIC"), min_idx, max_idx)
      )
  ) %>%
  bind_rows() %>%
  ggplot(aes(x = model, y = mean, col = model, shape = best_idx)) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = mean - se, ymax = mean + se),
    stat = "identity", position = "dodge", alpha = 0.4, col = "black", width = 0
  ) +
  facet_wrap(~metric, scales = "free") +
  scale_color_manual(values = multi.utils::cbpalette()) +
  scale_shape_manual(values = c(16, 15)) +
  guides(shape = "none") +
  labs(y = "Value", x = "", col = "") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 9),
    legend.margin = margin(0, 0, 0, 0)
  )

dev.off()

df_prop %>%
  mutate(
    dic = paste0(dic, " (", dic_se, ")"),
    waic = paste0(waic, " (", waic_se, ")"),
    cpo = paste0(cpo, " (", cpo_se, ")"),
  ) %>%
  select(-contains("se")) %>%
  mutate(
    model = fct_recode(model,
                       "L1" = "Model 1", "L2" = "Model 2", "L3" = "Model 3",
                       "L4" = "Model 4", "L5" = "Model 5", "L6" = "Model 6")
  ) %>%
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

#' #' Fitting countries individually
#'
#' #' Four categories
#'
#' #' All surveys with a transactional sex question
#' giftsvar_surveys <- available_surveys %>%
#'   filter(giftsvar == 1)
#'
#' files <- paste0("depends/", tolower(unique(giftsvar_surveys$iso3)), "_4-information-criteria.csv")
#' df_4 <- bind_rows(lapply(files, function(file) read_csv(file))) %>%
#'   mutate(age_group = "placeholder", indicator = "placeholder") %>%
#'   multi.utils::update_naming() %>%
#'   select(-age_group, -indicator)
#'
#' #' Countries with only one survey with a transactional sex question
#' iso3_4_single <- giftsvar_surveys %>%
#'   group_by(iso3) %>%
#'   summarise(count = length(unique(survey_id))) %>%
#'   filter(count == 1) %>%
#'   pull(iso3)
#'
#' #' Countries with multiple surveys with a transactional sex question
#' iso3_4_multi <- giftsvar_surveys %>%
#'   group_by(iso3) %>%
#'   summarise(count = length(unique(survey_id))) %>%
#'   filter(count > 1) %>%
#'   pull(iso3)
#'
#' #' Check that all the countries are covered
#' stopifnot(
#'   length(iso3_4_single) + length(iso3_4_multi) == length(unique(giftsvar_surveys$iso3))
#' )
#'
#' #' Four category
#'
#' #' Currently all the four category models are just spatial
#'
#' write_csv(df_4, "4-model-comparison.csv", na = "")
#'
#' pdf("4-model-comparison.pdf", h = 5, w = 6.25)
#'
#' ic_plot(df_4, ic = "dic")
#' ic_plot(df_4, ic = "waic")
#' ic_plot(df_4, ic = "cpo")
#'
#' dev.off()
#'
#' pdf("4-rank-comparison.pdf", h = 2.5, w = 6.25)
#'
#' rank_ic_plot(df_4)
#'
#' dev.off()
#'
#' create_latex_table(df_4, file_name = "4-model-comparison.txt")
#'
#' #' Three categories
#'
#' files <- paste0("depends/", tolower(unique(available_surveys$iso3)), "_3-information-criteria.csv")
#' df_3 <- bind_rows(lapply(files, function(file) read_csv(file))) %>%
#'   mutate(age_group = "placeholder", indicator = "placeholder") %>%
#'   multi.utils::update_naming() %>%
#'   select(-age_group, -indicator)
#'
#' #' Countries with only one survey
#' iso3_3_single <- available_surveys %>%
#'   group_by(iso3) %>%
#'   summarise(count = length(unique(survey_id))) %>%
#'   filter(count == 1) %>%
#'   pull(iso3)
#'
#' #' Countries with multiple surveys
#' iso3_3_multi <- available_surveys %>%
#'   group_by(iso3) %>%
#'   summarise(count = length(unique(survey_id))) %>%
#'   filter(count > 1) %>%
#'   pull(iso3)
#'
#' #' Single survey and three categories
#' df_3_single <- filter(df_3, iso3 %in% iso3_3_single)
#'
#' write_csv(df_3_single, "3-single-model-comparison.csv", na = "")
#'
#' pdf("3-single-model-comparison.pdf", h = 5, w = 6.25)
#'
#' ic_plot(df_3_single, ic = "dic")
#' ic_plot(df_3_single, ic = "waic")
#' ic_plot(df_3_single, ic = "cpo")
#'
#' dev.off()
#'
#' pdf("3-single-rank-comparison.pdf", h = 2.5, w = 6.25)
#'
#' rank_ic_plot(df_3_single)
#'
#' dev.off()
#'
#' create_latex_table(df_3, file_name = "3-single-model-comparison.txt")
#'
#' #' Multiple surveys and three categories
#' df_3_multi <- filter(df_3, iso3 %in% iso3_3_multi)
#'
#' write_csv(df_3_multi, "3-multi-model-comparison.csv", na = "")
#'
#' pdf("3-multi-model-comparison.pdf", h = 5, w = 6.25)
#'
#' ic_plot(df_3_multi, ic = "dic")
#' ic_plot(df_3_multi, ic = "waic")
#' ic_plot(df_3_multi, ic = "cpo")
#'
#' dev.off()
#'
#' pdf("3-multi-rank-comparison.pdf", h = 2.5, w = 6.25)
#'
#' rank_ic_plot(df_3_multi)
#'
#' dev.off()
#'
#' create_latex_table(df_3_multi, file_name = "3-multi-model-comparison.txt")
#'
#' #' Together plots
#' pdf("3-rank-comparison.pdf", h = 5, w = 6.25)
#'
#' cowplot::plot_grid(
#'   rank_ic_plot(df_3_single) +
#'     labs(title = "Countries with a single survey", y = "") +
#'     coord_flip(),
#'   rank_ic_plot(df_3_multi) +
#'     labs(title = "Countries with multiple surveys") +
#'     guides(fill = "none") +
#'     coord_flip(),
#'   ncol = 1,
#'   rel_heights = c(0.8, 1)
#' )
#'
#' dev.off()

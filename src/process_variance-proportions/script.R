#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("process_variance-proportions")
# setwd("src/process_variance-proportions")

#' This type of analysis falls under "Variance-based sensitivity analysis"
#' See also "Sobol method" or "Sobol indices"
#' Perhaps I should look into other interpretability measures e.g. Shapley values

#' Separate country fitting
iso3 <- multi.utils::priority_iso3()

#' The single survey estimates
files <- paste0("depends/", tolower(iso3), "_variance-proportions.csv")
df_aaa <- bind_rows(lapply(files, function(file) read_csv(file)))

#' Random effects created using area_idx_copy represent area_sur_idx
#' Put those values into area_sur_idx then remove the left-over columns
variance_effects <- c(
  "variance_cat_idx", "variance_age_idx", "variance_area_idx", "variance_sur_idx", "variance_area_sur_idx"
)

df_aaa <- df_aaa %>%
  #' Defensive programming
  mutate(
    variance_area_sur_idx = ifelse("variance_area_sur_idx" %in% names(.), variance_area_sur_idx, NA),
    percentage_variance_area_sur_idx = ifelse("percentage_variance_area_sur_idx" %in% names(.), percentage_variance_area_sur_idx, NA),
    variance_area_idx_copy = ifelse("variance_area_idx_copy" %in% names(.), variance_area_idx_copy, NA),
    percentage_variance_area_idx_copy = ifelse("percentage_variance_area_idx_copy" %in% names(.), percentage_variance_area_idx_copy, NA),
  ) %>%
  mutate(
    variance_area_sur_idx = case_when(
      is.na(variance_area_sur_idx) & !is.na(variance_area_idx_copy) ~ variance_area_idx_copy,
      TRUE ~ variance_area_sur_idx
    ),
    percentage_variance_area_sur_idx = case_when(
      is.na(percentage_variance_area_sur_idx) & !is.na(percentage_variance_area_idx_copy) ~ percentage_variance_area_idx_copy,
      TRUE ~ percentage_variance_area_sur_idx
    )
  ) %>%
  select(-variance_area_idx_copy, percentage_variance_area_idx_copy) %>%
  #' Reordering the columns as I'd prefer to see them appear
  select(iso3, model, all_of(variance_effects), total_variance, all_of(paste0("percentage_", variance_effects)))

write_csv(df_aaa, "aaa-variance-proportions.csv", na = "")

df_aaa <- df_aaa %>%
  mutate(
    iso3 = fct_recode(iso3,
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
    "Zimbabwe" = "ZWE"
    )
  )

fct_reorg <- function(fac, ...) {
  fct_recode(fct_relevel(fac, ...), ...)
}

#' When there is only one survey, we want to select Model 2, and when there are multiple, we want to select Model 4
single_survey <- df_aaa %>%
  group_by(iso3) %>%
  summarise(max = max(model)) %>%
  select(iso3, max) %>%
  filter(max == "Model 2") %>%
  pull(iso3)

model_selector <- function(iso3, model) {
  case_when(
    iso3 %in% single_survey ~ model == "Model 2",
    T ~ model == "Model 4"
  )
}

pdf("aaa-variance-proportions.pdf", h = 3.5, w = 6.25)

plotA <- df_aaa %>%
  filter(model_selector(iso3, model)) %>%
  select(model, iso3, starts_with("percentage_variance")) %>%
  mutate(iso3 = reorder(iso3, percentage_variance_area_idx)) %>%
  pivot_longer(starts_with("percentage_variance"), names_to = "random_effect", names_prefix = "percentage_variance_") %>%
  mutate(
    random_effect = fct_reorg(random_effect,
      "Category" = "cat_idx",
      "Age x Category" = "age_idx",
      "Survey x Category" = "sur_idx",
      "Area x Category" = "area_idx",
      "Area x Survey x Category" = "area_sur_idx"
    )
  ) %>%
  ggplot(aes(x = iso3, y = value, group = random_effect, fill = random_effect))+
    geom_bar(position = "fill", stat = "identity", alpha = 0.8, width = 0.85) +
    scale_fill_manual(values = multi.utils::cbpalette()[c(1, 2, 4, 3)]) +
    theme_minimal() +
    scale_y_continuous(labels = function(x) paste0(100 * x, "%")) +
    labs(x = "", y = "Posterior variance", fill = "Random effect") +
    coord_flip() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      plot.title = element_text(face = "bold"),
      legend.key.width = unit(1.5, "lines"),
      legend.key.height = unit(1, "lines"),
      strip.placement = "outside",
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 9)
    )

plotA

dev.off()

ggsave(
  "aaa-variance-proportions.png",
  plotA,
  width = 6.25, height = 3.5, units = "in", dpi = 300
)

df_aaa %>%
  filter(model_selector(iso3, model)) %>%
  select(iso3, starts_with("percentage_variance")) %>%
  rename(
    "Country" = "iso3",
    #' Can't write LaTeX math in gt yet
    "sigma-beta" = "percentage_variance_cat_idx",
    "sigma-alpha" = "percentage_variance_age_idx",
    "sigma-phi" = "percentage_variance_area_idx",
    "sigma-gamma" = "percentage_variance_sur_idx",
    "sigma-delta" = "percentage_variance_area_sur_idx"
  ) %>%
  select(-`sigma-delta`) %>%
  gt() %>%
  fmt_number(columns = -matches("Country"), rows = everything(), decimals = 3) %>%
  #' Replace NA with "-" as it looks better on a table
  fmt_missing(columns = everything(), rows = everything(), missing_text = "-") %>%
  cols_align(
    align = c("left"),
    columns = matches("Country")
  ) %>%
  as_latex() %>%
  as.character() %>%
  cat(file = "aaa-variance-proportions.txt")

df_aaa %>%
  group_by(model) %>%
  summarise(
    cat_idx = mean(percentage_variance_cat_idx),
    age_idx = mean(percentage_variance_age_idx),
    area_idx = mean(percentage_variance_area_idx),
    sur_idx = mean(percentage_variance_sur_idx),
    area_sur_idx = mean(percentage_variance_area_sur_idx)
  ) %>%
  write_csv("aaa-average-variance-proportions.csv", na = "")

#' Quantification of points discussed
#' Which are the countries with the highest and lowest proportion of variance explained by area?
df_aaa %>%
  filter(model_selector(iso3, model)) %>%
  select(iso3, percentage_variance_area_idx) %>%
  arrange(desc(percentage_variance_area_idx)) %>%
  mutate(percentage_variance_area_idx = signif(100 * percentage_variance_area_idx, 3))

#' The below is still in development

#' #' Joint country fitting
#' df <- read_csv("depends/variance-proportions.csv")
#'
#' #' What's the proportion of variance explained?
#' df %>%
#'   select(starts_with("percentage_")) %>%
#'   mutate(across(everything(), ~ 100 * round(.x, 3)))
#'
#' #' The uncertainty on that?
#' S <- 100
#' full_samples <- readRDS("depends/multi-sexbehav-sae-samples.rds")
#'
#' #' Just the hyperparameters
#' precision_samples <- lapply(full_samples, "[", "hyperpar")
#' precision_samples_matrix <- matrix(unlist(precision_samples), ncol = S)
#' precision_samples_df <- data.frame(t(precision_samples_matrix))
#' names(precision_samples_df) <- names(precision_samples[[1]][[1]])
#' variance_samples_df <- 1 / precision_samples_df
#'
#' variance_samples_df <- variance_samples_df %>%
#'   select(starts_with("Precision")) %>%
#'   rename_all(list(~ stringr::str_replace(., "Precision for ", "variance_"))) %>%
#'   mutate(total_variance = rowSums(., na.rm = TRUE)) %>%
#'   #' Create new columns with the percentage variance
#'   mutate(
#'     across(
#'       .cols = starts_with("variance"),
#'       .fns = list(percentage = ~ . / total_variance),
#'       .names = "{fn}_{col}"
#'     )
#'   ) %>%
#'   select(starts_with("percentage_")) %>%
#'   summarise(
#'     across(
#'       .cols = everything(),
#'       .fns = list(mean = mean, lower = ~ quantile(.x, probs = 0.025), upper = ~ quantile(.x, probs = 0.975))
#'     )
#'   ) %>%
#'   pivot_longer(
#'     cols = everything(),
#'     names_prefix = "percentage_variance_",
#'     names_to = "variable",
#'     values_to = "percentage_variance"
#'   ) %>%
#'   separate(variable, into = c("variable", "idx", "type")) %>%
#'   select(-idx) %>%
#'   pivot_wider(
#'     names_from = type,
#'     values_from = percentage_variance
#'   )
#'
#' write_csv(variance_samples_df, "variance-proportions-uncertainty.csv")
#'
#' #' Now to try the alternative method for looking within country when the model is fit jointly between all countries
#' #' Probably there is a better way to do this
#'
#' fits <- readRDS("depends/multi-sexbehav-sae-fits.rds")
#' fit <- fits[[1]]
#'
#' #' Use the results from the model as dictionary for variable to idx correspondence
#' #' * indicator: cat_idx
#' #' * iso3: iso3_idx
#' #' * year: year_idx
#' #' * area_id: area_idx
#' #' * age_group: age_idx
#' idx_dictionary <- read_csv("depends/multi-sexbehav-sae.csv") %>%
#'   select(indicator, cat_idx, age_group, age_idx, iso3, iso3_idx,  area_id, area_idx, year, year_idx) %>%
#'   mutate(
#'     age_idx_rowname = age_idx * cat_idx,
#'     iso3_idx_rowname = iso3_idx * cat_idx,
#'     area_idx_rowname = area_idx * cat_idx,
#'     year_idx_rowname = year_idx * cat_idx
#'   )
#'
#' cat_df <- fit$summary.random$cat_idx
#' age_df <- tibble::rownames_to_column(fit$summary.random$age_idx) %>% mutate(rowname = as.numeric(rowname))
#' iso3_df <- tibble::rownames_to_column(fit$summary.random$iso3_idx) %>% mutate(rowname = as.numeric(rowname))
#' area_df <- tibble::rownames_to_column(fit$summary.random$area_idx) %>% mutate(rowname = as.numeric(rowname))
#' year_df <- tibble::rownames_to_column(fit$summary.random$year_idx) %>% mutate(rowname = as.numeric(rowname))
#'
#' re_df <-idx_dictionary %>%
#'   left_join(
#'     select(cat_df, cat_idx = ID, cat_val = mean),
#'     by = "cat_idx"
#'   ) %>%
#'   left_join(
#'     select(age_df, age_idx_rowname = rowname, age_val = mean),
#'     by = "age_idx_rowname"
#'   ) %>%
#'   left_join(
#'     select(iso3_df, iso3_idx_rowname = rowname, iso3_val = mean),
#'     by = "iso3_idx_rowname"
#'   ) %>%
#'   left_join(
#'     select(area_df, area_idx_rowname = rowname, area_val = mean),
#'     by = "area_idx_rowname"
#'   ) %>%
#'   left_join(
#'     select(year_df, year_idx_rowname = rowname, year_val = mean),
#'     by = "year_idx_rowname"
#'   )
#'
#' re_df %>%
#'   group_by(iso3) %>%
#'   summarise(
#'     cat_idx = mean(abs(cat_val)),
#'     age_idx = mean(abs(age_val)),
#'     iso3_idx = mean(abs(iso3_val)),
#'     area_idx = mean(abs(area_val)),
#'     year_idx = mean(abs(year_val))
#'   )

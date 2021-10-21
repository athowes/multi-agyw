#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("process_variance-proportions")
# setwd("src/process_variance-proportions")

iso3 <- c("BWA", "CMR", "KEN", "LSO", "MOZ", "MWI", "NAM", "SWZ", "TZA", "UGA", "ZAF", "ZMB", "ZWE")

#' The single survey estimates
files <- paste0("depends/", tolower(iso3), "_variance-proportions.csv")
df <- bind_rows(lapply(files, function(file) read_csv(file)))

write_csv(df, "variance-proportions.csv", na = "")

df <- df %>%
  mutate(
    iso3 = fct_recode(iso3,
    "Botswana" = "BWA",
    "Cameroon" = "CMR",
    "Kenya" = "KEN",
    "Lesotho" = "LSO",
    "Mozambique" = "MOZ",
    "Malawi" = "MWI",
    "Namibia" = "NAM",
    "Swaziland" = "SWZ",
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

#' Don't have access to the surveys, going to use workaround whereby select Model 3 if there are only three models
#' When there is only one survey, we want to select Model 3, and when there are multiple, we want to select Model 6
single_survey <- df %>%
  group_by(iso3) %>%
  select(model) %>%
  unique() %>%
  count() %>%
  filter(n == 3)

model_selector <- function(iso3, model) {
  case_when(
    iso3 %in% single_survey$iso3 ~ model == "Model 3",
    T ~ model == "Model 6"
  )
}

#' A4 paper is 8-1/4 x 11-3/4
#' 8-1/4 take away 1 inch margins gives 6-1/4
pdf("variance-proportions.pdf", h = 3.5, w = 6.25)

cbpalette <- c("#56B4E9","#009E73", "#E69F00", "#F0E442","#0072B2","#D55E00","#CC79A7", "#999999")

df %>%
  filter(model_selector(iso3, model)) %>%
  select(model, iso3, starts_with("percentage_variance")) %>%
  pivot_longer(starts_with("percentage_variance"), names_to = "random_effect", names_prefix = "percentage_variance_") %>%
  mutate(
    random_effect = fct_reorg(random_effect,
      "Category" = "cat_idx",
      "Age x Category" = "age_idx",
      "Area x Category" = "area_idx",
      "Survey x Category" = "sur_idx"
    )
  ) %>%
  ggplot(aes(x = fct_rev(iso3), y = value, group = random_effect, fill = random_effect)) +
    geom_bar(position = "fill", stat = "identity", alpha = 0.8) +
    scale_fill_manual(values = cbpalette) +
    theme_minimal() +
    scale_y_continuous(labels = function(x) paste0(100 * x, "%")) +
    labs(x = "", y = "Posterior variance", fill = "") +
    coord_flip() +
    theme(
      plot.title = element_text(face = "bold"),
      legend.key.width = unit(2, "lines"),
      strip.placement = "outside"
    )

dev.off()

df %>%
  filter(model_selector(iso3, model)) %>%
  select(iso3, starts_with("percentage_variance")) %>%
  rename(
    "Country" = "iso3",
    #' Can't write LaTeX math in gt yet
    "sigma-beta" = "percentage_variance_cat_idx",
    "sigma-alpha" = "percentage_variance_age_idx",
    "sigma-phi" = "percentage_variance_area_idx",
    "sigma-gamma" = "percentage_variance_sur_idx",
  ) %>%
  gt() %>%
  fmt_number(columns = -matches("Country"), rows = everything(), decimals = 3) %>%
  cols_align(
    align = c("left"),
    columns = matches("Country")
  ) %>%
  as_latex() %>%
  as.character() %>%
  cat(file = "variance-proportions.txt")

df %>%
  group_by(model) %>%
  summarise(
    cat_idx = mean(percentage_variance_cat_idx),
    age_idx = mean(percentage_variance_age_idx),
    area_idx = mean(percentage_variance_area_idx),
    sur_idx = mean(percentage_variance_sur_idx)
  ) %>%
  write_csv("average-variance-proportions.csv", na = "")

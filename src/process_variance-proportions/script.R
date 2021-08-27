#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("process_variance-proportions")
# setwd("src/process_variance-proportions")

iso3 <- c("CMR", "KEN", "LSO", "MOZ", "MWI", "UGA", "ZMB", "ZWE")

#' The single survey estimates
files <- paste0("depends/", tolower(iso3), "_variance-proportions.csv")
df <- bind_rows(lapply(files, function(file) read_csv(file)))

write_csv(df, "variance-proportions.csv", na = "")

#' This has been included upstream
#' Waiting for first run through of results to delete
if(!("iso3" %in% colnames(df))) {
  df$iso3 <- rep(iso3, each = 9)
}

df <- df %>%
  mutate(
    iso3 = fct_recode(iso3,
    "Cameroon" = "CMR",
    "Kenya" = "KEN",
    "Lesotho" = "LSO",
    "Mozambique" = "MOZ",
    "Malawi" = "MWI",
    "Uganda" = "UGA",
    "Zambia" = "ZMB",
    "Zimbabwe" = "ZWE",
    )
  )

#' A4 paper is 8-1/4 x 11-3/4
#' 8-1/4 take away 1 inch margins gives 6-1/4
pdf("variance-proportions.pdf", h = 3.5, w = 6.25)

cbpalette <- c("#56B4E9","#009E73", "#E69F00", "#F0E442","#0072B2","#D55E00","#CC79A7", "#999999")

df %>%
  filter(model == "Model 9") %>%
  select(model, iso3, starts_with("percentage_variance")) %>%
  pivot_longer(starts_with("percentage_variance"), names_to = "random_effect", names_prefix = "percentage_variance_") %>%
  mutate(
    random_effect = fct_recode(random_effect,
      "Category" = "cat_idx",
      "Age x Category" = "age_cat_idx",
      "Area x Category" = "area_idx",
      "Survey x Category" = "sur_idx"
    )
  ) %>%
  ggplot(aes(x = iso3, y = value, group = random_effect, fill = random_effect)) +
    geom_bar(position = "fill", stat = "identity", alpha = 0.8) +
    scale_color_manual(values = cbpalette) +
    theme_minimal() +
    scale_y_continuous(labels = function(x) paste0(100 * x, "%")) +
    labs(x = "", y = "Posterior variance", fill = "") +
    theme(
      plot.title = element_text(face = "bold"),
      legend.key.width = unit(2, "lines"),
      strip.placement = "outside",
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
    )

dev.off()

df %>%
  filter(
    model == "Model 9",
    iso3 %in% iso3
  ) %>%
  select(iso3, starts_with("percentage_variance")) %>%
  rename(
    "Country" = "iso3",
    #' Can't write LaTeX math in gt yet
    "sigma-beta" = "percentage_variance_cat_idx",
    "sigma-alpha" = "percentage_variance_age_cat_idx",
    "sigma-phi" = "percentage_variance_area_idx",
    "sigma-gamma" = "percentage_variance_sur_idx",
  ) %>%
  gt() %>%
  fmt_number(columns = -matches("Country"), rows = everything(), decimals = 3) %>%
  as_latex() %>%
  as.character() %>%
  cat(file = "variance-proportions.txt")

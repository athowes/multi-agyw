#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("process_variance-proportions")
# setwd("src/process_variance-proportions")

iso3 <- c("CMR", "KEN", "LSO", "MOZ", "MWI", "UGA", "ZMB", "ZWE")

#' The single survey estimates
files <- paste0("depends/", tolower(iso3), "_variance-proportions.csv")
df <- bind_rows(lapply(files, function(file) read_csv(file)))

write_csv(df, "variance-proportions.csv", na = "")

#' Forgot to include this, will add and delete
if(!("iso3" %in% colnames(df))) {
  df$iso3 <- rep(iso3, each = 9)
}

pdf("variance-proportions.pdf", h = 5, w = 9)

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
    labs(title = "What proportion of the total variance in a country does each random effect explain?",
         x = "Country", y = "Proportion of posterior variance", fill = "Random Effect") +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "bottom",
      legend.key.width = unit(4, "lines"),
      strip.placement = "outside",
      strip.text.x = element_text(angle = 90, hjust = 0)
    )

dev.off()

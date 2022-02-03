#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("plot_age-variation")
# setwd("src/plot_age-variation")

df <- read_csv("depends/human-best-3-multinomial-smoothed-district-sexbehav.csv") %>%
  filter(age_group != "15-24")

df_age_country <- df %>%
  group_by(iso3, age_group, indicator) %>%
  summarise(estimate_smoothed = mean(estimate_smoothed, na.rm = TRUE))

df_age <- df %>%
  group_by(age_group, indicator) %>%
  summarise(estimate_smoothed = mean(estimate_smoothed, na.rm = TRUE))

pdf("age-variation.pdf", h = 3.5, w = 6.25)

cbpalette <- c("#56B4E9","#009E73", "#E69F00", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")

ggplot(df_age_country, aes(y = age_group, x = estimate_smoothed, fill = indicator)) +
  ggridges::geom_density_ridges(alpha = 0.7, col = NA) +
  scale_fill_manual(values = cbpalette) +
  # facet_wrap(~indicator) +
  theme_minimal() +
  scale_x_continuous(labels = function(x) paste0(100 * x, "%")) +
  scale_y_discrete(expand = expansion(mult = c(0.05, 0))) +
  xlim(0, 1) +
  labs(y = "Age group", x = "Proportion", fill = "Category")
  # theme(
  #   legend.position = "bottom",
  #   legend.key.width = unit(1, "lines")
  # )

dev.off()

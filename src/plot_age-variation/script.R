#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("plot_age-variation")
# setwd("src/plot_age-variation")

df <- read_csv("depends/adjust-best-3p1-multi-sexbehav-sae.csv") %>%
  multi.utils::update_naming() %>%
  filter(
    age_group != "15-24",
    indicator != "Non-regular or multiple partners(s) +"
  )

#' Add region column
#' Defined based on the UN geoscheme for Africa
#' https://en.wikipedia.org/wiki/United_Nations_geoscheme_for_Africa
region_key <- c(
  "Botswana" = "Southern",
  "Cameroon" = "Central",
  "Kenya" = "Eastern",
  "Lesotho" = "Southern",
  "Mozambique" = "Eastern",
  "Malawi" = "Eastern",
  "Namibia" = "Southern",
  "Eswatini" = "Southern",
  "Tanzania" = "Eastern",
  "Uganda" = "Eastern",
  "South Africa" = "Southern",
  "Zambia" = "Eastern",
  "Zimbabwe" = "Eastern"
) %>%
  as.data.frame() %>%
  rename("region" = ".") %>%
  tibble::rownames_to_column("iso3")

df <- df %>%
  left_join(region_key, by = "iso3")

df_age_country <- df %>%
  group_by(iso3, age_group, indicator, region) %>%
  summarise(estimate_smoothed = mean(estimate_smoothed, na.rm = TRUE))

df_age <- df %>%
  group_by(age_group, indicator, region) %>%
  summarise(estimate_smoothed = mean(estimate_smoothed, na.rm = TRUE))

pdf("age-variation.pdf", h = 3.5, w = 6.25)

plotA <- ggplot(df_age_country, aes(y = age_group, x = estimate_smoothed, fill = indicator)) +
  ggridges::geom_density_ridges(alpha = 0.7, col = NA) +
  scale_fill_manual(values = multi.utils::cbpalette()) +
  # facet_wrap(~indicator) +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_y_discrete(expand = expansion(mult = c(0.05, 0))) +
  labs(y = "Age group", x = "Proportion", fill = "Risk group") +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(1, "lines"),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 9)
  )

plotA

dev.off()

ggsave(
  "age-variation.png",
  plotA,
  width = 6.25, height = 3.5, units = "in", dpi = 300
)

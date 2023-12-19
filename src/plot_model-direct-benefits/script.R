#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("plot_model-direct-benefits")
# setwd("src/plot_model-direct-benefits")

df <- read_csv("depends/tza_multi-sexbehav-sae.csv")
areas <- read_sf(paste0("depends/tza_areas.geojson"))

df <- df %>%
  filter(
    survey_id == "TZA2010DHS",
    age_group == "Y020_024",
    model == "Model 4",
    area_id != "TZA",
    indicator == "sexcohab"
  ) %>%
  mutate(
    age_group = fct_recode(age_group, "20-24" = "Y020_024"),
    indicator = fct_recode(indicator, "Cohabiting partner" = "sexcohab")
  ) %>%
  pivot_longer(
    cols = c(starts_with("estimate")),
    names_to = c(".value", "source"),
    names_pattern = "(.*)\\_(.*)"
  ) %>%
  mutate(
    source = fct_recode(source,
      "Direct" = "raw",
      "Modelled" = "smoothed"
    )
  ) %>%
  left_join( #' Use this to make it an sf again
    select(areas, area_id),
    by = "area_id"
  ) %>%
  st_as_sf()

#' Which of the districts are missing the raw survey estimate?
missing_districts <- df %>%
  filter(
    source == "Raw",
    is.na(n_clusters)
  ) %>%
  select(area_name)

missing_districts$area_name %>% sort()

pdf("model-direct-benefits.pdf", h = 3.5, w = 6.25)

plotA <- ggplot(df, aes(fill = estimate)) +
  geom_sf(size = 0.1) +
  scale_fill_viridis_c(option = "C", label = label_percent(), na.value = "#E6E6E6") +
  facet_grid(indicator ~ source) +
  labs(fill = "Proportion\nof women\n20-24\ncohabiting\n(2010)\n") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    # strip.text = element_text(face = "bold", size = 9),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 9),
    plot.title = element_text(face = "bold"),
    legend.key.width = unit(1, "lines"),
    legend.position = "left",
    strip.text.y = element_blank()
  )

plotA

dev.off()

ggsave(
  "model-direct-benefits.png",
  plotA,
  width = 6.25, height = 3.5, units = "in", dpi = 300
)

#' This is a potential plot to compare the raw and smoothed estimates using a stacked barplot
#' I haven't got it to look great though so far, so not including
#
# ggplot(df, aes(fill = indicator, y = estimate, x = fct_rev(area_name))) +
#   geom_bar(position = "stack", stat = "identity") +
#   facet_wrap(~source) +
#   scale_fill_manual(values = multi.utils::cbpalette()) +
#   scale_y_continuous(labels = scales::percent) +
#   labs(x = "", y = "", fill = "Category") +
#   coord_flip() +
#   theme_minimal() +
#   theme(
#     legend.position = "bottom",
#     legend.key.width = unit(1.5, "lines"),
#   )

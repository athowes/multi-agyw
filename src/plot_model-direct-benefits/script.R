#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("plot_model-direct-benefits")
# setwd("src/plot_model-direct-benefits")

df <- read_csv("depends/tza_multinomial-smoothed-district-sexbehav.csv")
areas <- read_sf(paste0("depends/tza_areas.geojson"))

df <- df %>%
  filter(
    survey_id == "TZA2010DHS",
    age_group == "Y020_024",
    model == "Model 6",
    area_id != "TZA",
  ) %>%
  mutate(
    age_group = fct_recode(age_group,
      "20-24" = "Y020_024",
    ),
    indicator = fct_recode(indicator,
      "No sex (past 12 months)" = "nosex12m",
      "Cohabiting partner" = "sexcohab",
      "Nonregular partner(s)" = "sexnonregplus",
    )
  ) %>%
  pivot_longer(
    cols = c(starts_with("estimate")),
    names_to = c(".value", "source"),
    names_pattern = "(.*)\\_(.*)"
  ) %>%
  mutate(
    source = fct_recode(source,
      "Raw" = "raw",
      "Smoothed" = "smoothed"
    )
  ) %>%
  left_join( #' Use this to make it an sf again
    select(areas, area_id),
    by = "area_id"
  ) %>%
  st_as_sf()

#' Which of the Zambian districts are missing the raw survey estimate?
missing_districts <- df %>%
  filter(
    indicator == "No sex (past 12 months)",
    source == "Raw",
    is.na(n_clusters)
  ) %>%
  select(area_name)

missing_districts$area_name %>% sort()

pdf("model-direct-benefits-v.pdf", h = 8, w = 6.25)

ggplot(df, aes(fill = estimate)) +
  geom_sf(size = 0.1, colour = scales::alpha("grey", 0.25)) +
  scale_fill_viridis_c(option = "C", label = label_percent(), na.value = "#E6E6E6") +
  facet_grid(indicator ~ source) +
  labs(fill = "Estimated proportion") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold", size = rel(0.85)),
    legend.title = element_text(size = rel(0.7)),
    legend.text = element_text(size = rel(0.7)),
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.key.width = unit(4, "lines"),
  )

dev.off()

pdf("model-direct-benefits-h.pdf", h = 6.25, w = 8)

ggplot(df, aes(fill = estimate)) +
  geom_sf(size = 0.1, colour = scales::alpha("grey", 0.25)) +
  scale_fill_viridis_c(option = "C", label = label_percent(), na.value = "#E6E6E6") +
  facet_grid(source ~ indicator) +
  labs(fill = "Estimated proportion") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold", size = rel(0.85)),
    legend.title = element_text(size = rel(0.7)),
    legend.text = element_text(size = rel(0.7)),
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.key.width = unit(4, "lines"),
  )

dev.off()

#' This is a potential plot to compare the raw and smoothed estimates using a stacked barplot
#' I haven't got it to look great though so far, so not including
# cbpalette <- c("#56B4E9","#009E73", "#E69F00", "#F0E442","#0072B2","#D55E00","#CC79A7", "#999999")
#
# ggplot(df, aes(fill = indicator, y = estimate, x = fct_rev(area_name))) +
#   geom_bar(position = "stack", stat = "identity") +
#   facet_wrap(~source) +
#   scale_fill_manual(values = cbpalette) +
#   scale_y_continuous(labels = scales::percent) +
#   labs(x = "", y = "", fill = "Category") +
#   coord_flip() +
#   theme_minimal() +
#   theme(
#     legend.position = "bottom",
#     legend.key.width = unit(1.5, "lines"),
#   )

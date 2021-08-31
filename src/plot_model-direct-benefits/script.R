#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("plot_model-direct-benefits")
# setwd("src/plot_model-direct-benefits")

df <- read_csv("depends/mwi_multinomial-smoothed-district-sexbehav.csv")

areas <- read_sf(paste0("depends/mwi_areas.geojson"))

df <- df %>%
  filter(
    survey_id == "MWI2015DHS",
    age_group == "Y020_024",
    model == "Model 9",
    area_name != "Malawi",
  ) %>%
  mutate(
    age_group = fct_recode(age_group,
      "15-19" = "Y015_019",
      "20-24" = "Y020_024",
      "25-29" = "Y025_029"
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

pdf("model-direct-benefits.pdf", h = 10, w = 6.25)

cbpalette <- c("#56B4E9","#009E73", "#E69F00", "#F0E442","#0072B2","#D55E00","#CC79A7", "#999999")

A <- ggplot(df, aes(fill = indicator, y = estimate, x = fct_rev(area_name))) +
  geom_bar(position = "stack", stat = "identity") +
  facet_wrap(~source) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_fill_manual(values = cbpalette) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "", y = "", fill = "Category") +
  coord_flip() +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(1.5, "lines"),
  )

B <- ggplot(df, aes(fill = estimate)) +
  geom_sf(size = 0.1) +
  scale_fill_viridis_c(option = "C", label = label_percent()) +
  facet_grid(source ~ indicator) +
  labs(fill = "") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.key.width = unit(4, "lines")
  )

cowplot::plot_grid(A, B, ncol = 1)

dev.off()

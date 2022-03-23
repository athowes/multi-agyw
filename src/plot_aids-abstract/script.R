#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("plot_aids-abstract")
# setwd("src/plot_plot_aids-abstract")

df_3p1 <- read_csv("depends/adjust-best-3p1-multi-sexbehav-sae.csv")
pop <- read_csv("depends/interpolated_population.csv")

#' Add population and updating naming
df_3p1 <- df_3p1 %>%
  left_join(
    filter(pop, sex == "female"),
    by = c("area_id", "year", "age_group")
  ) %>%
  rename(population_mean = population) %>%
  multi.utils::update_naming()

#' The following section is copied mostly from plot_within-between-country-variation

#' To add region column defined based on the UN geoscheme for Africa
#' https://en.wikipedia.org/wiki/United_Nations_geoscheme_for_Africa
region_key <- c(
  "Botswana" = "South",
  "Cameroon" = "Middle",
  "Kenya" = "East",
  "Lesotho" = "South",
  "Mozambique" = "East",
  "Malawi" = "East",
  "Namibia" = "South",
  "Eswatini" = "South",
  "Tanzania" = "East",
  "Uganda" = "East",
  "South Africa" = "South",
  "Zambia" = "East",
  "Zimbabwe" = "East"
) %>%
  as.data.frame() %>%
  rename("region" = ".") %>%
  tibble::rownames_to_column("iso3")

df_3p1 <- df_3p1 %>%
  filter(
    age_group %in% c("20-24", "25-29"),
    year == "2018"
  ) %>%
  #' Aggregate up to 20-29
  group_by(iso3, area_id, indicator) %>%
  mutate(population_mean = ifelse(is.na(population_mean), 1, population_mean)) %>%
  summarise(
    estimate_smoothed = sum(estimate_smoothed * population_mean) / sum(population_mean),
    population_mean = sum(population_mean)
  ) %>%
  mutate(age_group = "20-29") %>%
  ungroup() %>%
  #' Add region column
  left_join(region_key, by = "iso3")

df_3p1_subnational <- df_3p1

df_3p1_national <- df_3p1 %>%
  group_by(iso3, age_group, indicator, region) %>%
  summarise(
    estimate_smoothed = sum(estimate_smoothed * population_mean, na.rm = TRUE) / sum(population_mean, na.rm = TRUE)
  )

df_3p1_national_sort <- df_3p1_national %>%
  select(iso3, region) %>%
  unique() %>%
  group_by(region) %>%
  arrange(iso3, .by_group = TRUE) %>%
  ungroup() %>%
  mutate(iso3_sort_order = row_number()) %>%
  select(iso3, iso3_sort_order)

df_3p1_subnational <- df_3p1_subnational %>%
  left_join(
    df_3p1_national_sort,
    by = "iso3"
  )

#' For the AIDS abstract, filter down to only 20-29 and cohabiting or non-regular partners
plotB <- df_3p1_subnational %>%
  filter(
    indicator %in% c("Cohabiting partner", "Nonregular partner(s)")
  ) %>%
  mutate(
    iso3 = reorder(iso3, iso3_sort_order)
  ) %>%
  ggplot(aes(x = fct_rev(iso3), y = estimate_smoothed, col = region)) +
  geom_jitter(width = 0.1, alpha = 0.6, shape = 20) +
  geom_point(
    data = df_3p1_national %>%
      filter(
        indicator %in% c("Cohabiting partner", "Nonregular partner(s)")
      ),
    aes(x = fct_rev(iso3), y = estimate_smoothed),
    shape = 21,
    size = 2,
    fill = "white",
    col = "black",
    alpha = 0.9
  ) +
  facet_grid(age_group ~  indicator) +
  scale_color_manual(values = multi.utils::cbpalette()) +
  scale_y_continuous(labels = function(x) paste0(100 * x, "%")) +
  coord_flip() +
  labs(
    x = "", y = "", col = "Regions of sub-Saharan Africa",
    caption =
      expression(
        paste(bold("Not sexually active"), " (not shown) + ",
              bold("Cohabiting partner"), " + ",
              bold("Nonregular partner(s)"), " + ",
              bold("FSW"), " (not shown) = 100%")
      )
  ) +
  guides(colour = guide_legend(override.aes = list(alpha = 0.9, size = 5))) +
  theme_minimal() +
  theme(
    panel.spacing = unit(1, "lines"),
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    strip.text.y = element_blank(),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 9)
  )

#' The following section is copied from plot_continental-map

areas <- readRDS("depends/areas.rds")
national_areas <- readRDS("depends/national_areas.rds")

df_3p1_subnational_sf <- df_3p1_subnational %>%
  left_join(
    select(areas, area_id, geometry),
    by = "area_id"
  ) %>%
  st_as_sf()

#' Using these just to show missing data for the countries we don't consider in the analysis
national_areas <- national_areas %>%
  filter(
    !(GID_0 %in% multi.utils::priority_iso3()),
    #' These are just chosen manually by looking at countries between CMR and the rest on a map
    GID_0 %in% c("AGO", "DRC", "CAF", "COD", "COG", "GAB", "GNQ", "RWA", "BDI")
  ) %>%
  rename(
    iso3 = NAME_0, #' Weird but OK
  ) %>%
  select(-GID_0)

df_3p1_national_areas <- crossing(
  indicator = as.factor(c("Cohabiting partner", "Nonregular partner(s)")),
  age_group = c("20-29"),
  iso3 = unique(national_areas$iso3)
) %>%
  left_join(national_areas, by = "iso3")

df_3p1_subnational_sf <- bind_rows(
  df_3p1_subnational_sf,
  df_3p1_national_areas
)

plotA <- df_3p1_subnational_sf %>%
  filter(indicator %in% c("Cohabiting partner", "Nonregular partner(s)")) %>%
  ggplot(aes(fill = estimate_smoothed)) +
    geom_sf(size = 0.1, colour = scales::alpha("grey", 0.25)) +
    scale_fill_viridis_c(option = "C", label = label_percent(), na.value = "#E6E6E6") +
    facet_grid(age_group ~ indicator) +
    labs(
      fill = "Proportion\nof women\n20-29"
    ) +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold"),
      legend.key.width = unit(1, "lines"),
      legend.position = "left",
      strip.text.y = element_blank(),
      panel.spacing = unit(1.5, "lines"),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 9)
    )

pdf("aids-abstract.pdf", h = 7, w = 6.25)

cowplot::plot_grid(plotA, plotB, ncol = 1, rel_heights = c(1, 1.1), align = "v")

dev.off()

#' Save version for poster
ggsave(
  "aids-abstract.png",
  cowplot::plot_grid(plotA, plotB, ncol = 1, rel_heights = c(1, 1.1), align = "v"),
  width = 6, height = 6, units = "in", dpi = 300
)

#' Separate versions are useful for presentations
pdf("aids-abstract-A.pdf", h = 3.5, w = 6.25)

plotA

dev.off()

pdf("aids-abstract-B.pdf", h = 3.5, w = 6.25)

plotB

dev.off()

#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("plot_aids-abstract")
# setwd("src/plot_plot_aids-abstract")

df <- read_csv("depends/human-best-3-multinomial-smoothed-district-sexbehav.csv")

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

df <- df %>%
  filter(age_group %in% c("20-24", "25-29")) %>%
  mutate(
    #' Assuming the survey_id is structured as ISO2000DHS
    year = substr(survey_id, 4, 7),
  ) %>%
  #' Only the most recent survey in each year
  group_by(iso3) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  #' Aggregate up to 20-29
  group_by(iso3, area_id, indicator) %>%
  mutate(population_mean = ifelse(is.na(population_mean), 1, population_mean)) %>%
  summarise(estimate_smoothed = sum(estimate_smoothed * population_mean) / sum(population_mean)) %>%
  mutate(age_group = "20-29") %>%
  ungroup() %>%
  #' Add region column
  left_join(region_key, by = "iso3")

priority_iso3 <- c("BWA", "CMR", "KEN", "LSO", "MOZ", "MWI", "NAM", "SWZ", "TZA", "UGA", "ZAF", "ZMB", "ZWE")

df_subnational <- df %>%
  filter(!(area_id %in% c(priority_iso3)))

df_national <- setdiff(df, df_subnational)

#' Countries that are missing a national level aggregate
#' This is a temporary solution and we should go back over and get these aggregates in properly
missing_national <- df_national %>%
  filter(is.na(estimate_smoothed)) %>%
  pull(iso3) %>%
  unique()

#' Overwriting NAs left_join (there must be a better way to do this, looks a lot simpler in data.table)
df_national <- left_join(
  df_national,
  df_subnational %>%
    filter(iso3 %in% missing_national) %>%
    group_by(iso3, indicator) %>%
    summarise(estimate_smoothed = mean(estimate_smoothed, na.rm = TRUE)),
  by = c("iso3", "indicator")
) %>%
  within(., estimate_smoothed.x <- ifelse(!is.na(estimate_smoothed.y), estimate_smoothed.y, estimate_smoothed.x)) %>%
  select(-estimate_smoothed.y) %>%
  rename(estimate_smoothed = estimate_smoothed.x)

df_national_sort <- df_national %>%
  select(iso3, region) %>%
  unique() %>%
  group_by(region) %>%
  arrange(iso3, .by_group = TRUE) %>%
  ungroup() %>%
  mutate(iso3_sort_order = row_number()) %>%
  select(iso3, iso3_sort_order)

df_subnational <- df_subnational %>%
  left_join(
    df_national_sort,
    by = "iso3"
  )

#' For the AIDS abstract, filter down to only 20-29 and cohabiting or non-regular partners

plotB <- df_subnational %>%
  filter(
    indicator %in% c("Cohabiting partner", "Nonregular partner(s)")
  ) %>%
  mutate(
    iso3 = reorder(iso3, iso3_sort_order)
  ) %>%
  ggplot(aes(x = fct_rev(iso3), y = estimate_smoothed, col = region)) +
  geom_jitter(width = 0.1, alpha = 0.6, shape = 20) +
  geom_point(
    data = df_national %>%
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
        paste(bold("Not sexually active"), " (not shown) + ", bold("Cohabiting partner"), " + ", bold("Nonregular partner(s)"), " = 100%")
      )
  ) +
  guides(colour = guide_legend(override.aes = list(alpha = 0.9, size = 5))) +
  theme_minimal() +
  theme(
    panel.spacing = unit(1, "lines"),
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    strip.text.y = element_blank()
  )

#' The following section is copied from plot_continental-map

areas <- readRDS("depends/areas.rds")
national_areas <- readRDS("depends/national_areas.rds")

df_subnational_sf <- df_subnational %>%
  left_join(
    select(areas, area_id, geometry),
    by = "area_id"
  ) %>%
  st_as_sf()

#' Using these just to show missing data for the countries we don't consider in the analysis
national_areas <- national_areas %>%
  filter(
    !(GID_0 %in% priority_iso3),
    #' These are just chosen manually by looking at countries between CMR and the rest on a map
    GID_0 %in% c("AGO", "DRC", "CAF", "COD", "COG", "GAB", "GNQ", "RWA", "BDI")
  ) %>%
  rename(
    iso3 = NAME_0, #' Weird but OK
  ) %>%
  select(-GID_0)

df_national_areas <- crossing(
  indicator = as.factor(c("Cohabiting partner", "Nonregular partner(s)")),
  age_group = c("20-29"),
  iso3 = unique(national_areas$iso3)
) %>%
  left_join(national_areas, by = "iso3")

df_subnational_sf <- bind_rows(
  df_subnational_sf,
  df_national_areas
)

plotA <- df_subnational_sf %>%
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
      panel.spacing = unit(1.5, "lines")
    )

pdf("aids-abstract.pdf", h = 7, w = 6.25)

cowplot::plot_grid(plotA, plotB, ncol = 1, rel_heights = c(1, 1.1), align = "v")

dev.off()

#' Separate versions are useful for presentations

pdf("aids-abstract-A.pdf", h = 3.5, w = 6.25)

plotA

dev.off()

pdf("aids-abstract-B.pdf", h = 3.5, w = 6.25)

plotB

dev.off()




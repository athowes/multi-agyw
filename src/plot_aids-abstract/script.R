#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("plot_aids-abstract")
# setwd("src/plot_plot_aids-abstract")

df <- read_csv("depends/best-3-multinomial-smoothed-district-sexbehav.csv")

#' The following section is copied from plot_within-between-country-variation

df <- df %>%
  filter(
    age_group != "Y015_024"
  ) %>%
  mutate(
    #' Assuming the survey_id is structured as ISO2000DHS
    year = substr(survey_id, 4, 7),
    #' Labels for plot
    age_group = fct_relevel(age_group, "Y015_024", after = 3) %>%
      fct_recode(
        "15-19" = "Y015_019",
        "20-24" = "Y020_024",
        "25-29" = "Y025_029",
        "15-24" = "Y015_024"
      ),
    indicator =
      fct_recode(indicator,
                 "No sex (past 12 months)" = "nosex12m",
                 "Cohabiting partner" = "sexcohab",
                 "Nonregular partner(s)" = "sexnonregplus"
      ),
    iso3 =
      fct_recode(iso3,
                 "Botswana" = "BWA",
                 "Cameroon" = "CMR",
                 "Kenya" = "KEN",
                 "Lesotho" = "LSO",
                 "Mozambique" = "MOZ",
                 "Malawi" = "MWI",
                 "Namibia" = "NAM",
                 "Eswatini" = "SWZ",
                 "Tanzania" = "TZA",
                 "Uganda" = "UGA",
                 "South Africa" = "ZAF",
                 "Zambia" = "ZMB",
                 "Zimbabwe" = "ZWE"
      )
  ) %>%
  #' Only the most recent survey in each year
  group_by(iso3) %>%
  filter(year == max(year)) %>%
  ungroup()

df_subnational <- df %>%
  filter(
    !(area_id %in% c(
      "BWA", "CMR", "KEN", "LSO", "MOZ", "MWI", "NAM",
      "SWZ", "TZA", "UGA", "ZAF", "ZMB", "ZWE"
    )
    )
  )

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
    group_by(iso3, age_group, indicator) %>%
    summarise(estimate_smoothed = mean(estimate_smoothed)),
  by = c("iso3", "age_group", "indicator")
) %>%
  within(., estimate_smoothed.x <- ifelse(!is.na(estimate_smoothed.y), estimate_smoothed.y, estimate_smoothed.x)) %>%
  select(-estimate_smoothed.y) %>%
  rename(estimate_smoothed = estimate_smoothed.x)

#' Add region column
#' Defined based on the UN geoscheme for Africa
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

df_subnational <- df_subnational %>%
  left_join(region_key, by = "iso3")

df_national <- df_national %>%
  left_join(region_key, by = "iso3")

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

cbpalette <- c("#56B4E9","#009E73", "#E69F00", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")

#' For the AIDS abstract, filter down to only 20-29 and cohabiting or non-regular partners

plotA <- df_subnational %>%
  filter(
    age_group %in% c("20-24", "25-29"),
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
        age_group %in% c("20-24", "25-29"),
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
  scale_color_manual(values = cbpalette) +
  scale_y_continuous(labels = function(x) paste0(100 * x, "%")) +
  coord_flip() +
  labs(x = "", y = "Proportion", col = "UN geoscheme region") +
  guides(colour = guide_legend(override.aes = list(alpha = 0.9, size = 5))) +
  theme_minimal() +
  theme(
    panel.spacing = unit(1.5, "lines"),
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )

#' The following section is copied from plot_continental-map

areas <- readRDS("depends/areas.rds")
national_areas <- readRDS("depends/national_areas.rds")

df <- df %>%
  left_join(
    select(areas, area_id, geometry),
    by = "area_id"
  ) %>%
  st_as_sf()

priority_iso3 <- c("BWA", "CMR", "KEN", "LSO", "MOZ", "MWI", "NAM", "SWZ", "TZA", "UGA", "ZAF", "ZMB", "ZWE")

df_subnational <- df %>%
  filter(!(area_id %in% priority_iso3))

df_national <- setdiff(df, df_subnational)

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
  indicator = as.factor(c("No sex (past 12 months)" , "Cohabiting partner", "Nonregular partner(s)")),
  age_group = c("15-19", "20-24", "25-29"),
  iso3 = unique(national_areas$iso3)
) %>%
  left_join(national_areas, by = "iso3")

df_subnational <- bind_rows(
  df_subnational,
  df_national_areas
)

plotB <- df_subnational %>%
  filter(
    age_group %in% c("20-24", "25-29"),
    indicator %in% c("Cohabiting partner", "Nonregular partner(s)")
  ) %>%
  ggplot(aes(fill = estimate_smoothed)) +
    geom_sf(size = 0.1) +
    scale_fill_viridis_c(option = "C", label = label_percent(), na.value = "#E6E6E6") +
    facet_grid(age_group ~ indicator) +
    labs(fill = "Estimated proportion") +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold"),
      legend.position = "bottom",
      legend.key.width = unit(2, "lines")
    )

pdf("aids-abstract.pdf", h = 7, w = 8.5)

cowplot::plot_grid(plotA, plotB, ncol = 2)

dev.off()

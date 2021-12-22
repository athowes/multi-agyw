#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("plot_continental-map")
# setwd("src/plot_continental-map")

df <- read_csv("depends/best-3-multinomial-smoothed-district-sexbehav.csv")
areas <- readRDS("depends/areas.rds")
national_areas <- readRDS("depends/national_areas.rds")

df <- df %>%
  filter(
    age_group != "Y015_024",
  ) %>%
  mutate(
    #' Assuming the survey_id is structured as ISO2000DHS
    year = substr(survey_id, 4, 7),
    #' Labels for plot
    age_group =
      fct_recode(age_group,
        "15-19" = "Y015_019",
        "20-24" = "Y020_024",
        "25-29" = "Y025_029",
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
                 "Swaziland" = "SWZ",
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
  ungroup() %>%
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
  filter(!(GID_0 %in% priority_iso3)) %>%
  rename(
    iso3 = NAME_0, #' Weird but OK
  ) %>%
  select(-GID_0)

df_national_areas <- crossing(
  indicator = c("No sex (past 12 months)" , "Cohabiting partner", "Nonregular partner(s)"),
  age_group = c("15-19", "20-24", "25-29"),
  iso3 = unique(national_areas$iso3)
) %>%
  left_join(national_areas, by = "iso3")

df_subnational <- bind_rows(
  df_subnational,
  df_national_areas
)

pdf("continential-map.pdf", h = 8, w = 6.25)

ggplot(df_subnational, aes(fill = estimate_smoothed)) +
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
    legend.key.width = unit(4, "lines")
  )

dev.off()

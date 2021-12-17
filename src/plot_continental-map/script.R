#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("plot_continental-map")
# setwd("src/plot_continental-map")

df <- read_csv("depends/best-3-multinomial-smoothed-district-sexbehav.csv")
areas <- readRDS("depends/areas.rds")

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

df_subnational <- df %>%
  filter(
    !(area_id %in% c(
      "BWA", "CMR", "KEN", "LSO", "MOZ", "MWI", "NAM",
      "SWZ", "TZA", "UGA", "ZAF", "ZMB", "ZWE"
    )
    )
  )

df_national <- setdiff(df, df_subnational)

pdf("continential-map.pdf", h = 11.75, w = 8.25)

ggplot(df_subnational, aes(fill = estimate_smoothed)) +
  geom_sf(size = 0.1) +
  scale_fill_viridis_c(option = "C", label = label_percent()) +
  facet_grid(age_group ~ indicator) +
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

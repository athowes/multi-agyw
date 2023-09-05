continental_map <- function(df, areas, national_areas) {

  df <- df %>%
    filter(!age_group %in%  c("Y030_034","Y035_039","Y040_044","Y045_049")) %>%
    mutate(
      age_group = fct_recode(age_group,
                             "15-19" = "Y015_019",
                             "20-24" = "Y020_024",
                             "25-29" = "Y025_029",
      ),
      indicator = fct_recode(indicator,
                             "Not sexually active" = "nosex12m",
                             "One cohabiting partner" = "sexcohab",
                             "Non-regular or multiple partners(s) +" = "sexnonregplus",
                             "Non-regular or multiple partner(s)" = "sexnonreg",
                             "FSW" = "sexpaid12m"
      )) %>%
    #' Only the most recent survey in each year
    group_by(iso3) %>%
    filter(year == max(year),
           iso3 != "HTI") %>%
    ungroup() %>%
    left_join(
      select(areas, area_id, geometry),
      by = "area_id"
    ) %>%
    st_as_sf()

  priority_iso3 <- c("BWA", "CMR", "KEN", "LSO", "MOZ", "MWI", "NAM", "SWZ",
                     "TZA", "UGA", "ZAF", "ZMB", "ZWE",
                     "AGO", "BDI", "COD", "GAB", "RWA", "ETH", "TCD",
                     "CIV", "GHA", "GIN", "LBR", "MLI",
                     "NER", "SLE", "TGO", "BFA", "COG", "CAF")

  #' Countries that I want to show on the plot but we don't have data for
  #' These are just chosen manually by looking at countries between CMR and the rest on a map
  missing_iso3 <- c("GNQ", "NGA", "BEN","SSD")

  df_subnational <- filter(df, !(area_id %in% priority_iso3))
  df_national <- setdiff(df, df_subnational)

  missing_national_areas <- national_areas %>%
    filter(area_id %in% missing_iso3) %>%
    rename(iso3 = area_id)

  df_national_areas <- crossing(
    indicator = unique(df_national$indicator),
    age_group = unique(df_national$age_group),
    iso3 = unique(missing_national_areas$iso3)
  ) %>%
    left_join(missing_national_areas, by = "iso3")

  df_subnational <- bind_rows(
    df_subnational,
    df_national_areas
  )

  ggplot(df_subnational, aes(fill = estimate_smoothed)) +
    geom_sf(size = 0.05, colour = scales::alpha("grey", 0.1)) +
    geom_sf(data = filter(national_areas, area_id %in% c(priority_iso3, missing_iso3)),
            aes(geometry = geometry), fill = NA, size = 0.2) +
    scale_fill_viridis_c(option = "C", label = label_percent(), na.value = "#E6E6E6") +
    facet_grid(age_group ~ indicator, labeller = labeller(indicator = label_wrap_gen(20))) +
    labs(fill = "Estimated proportion") +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold"),
      legend.position = "bottom",
      legend.key.width = unit(4, "lines"),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 9)
    )
}

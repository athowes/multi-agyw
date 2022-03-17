continental_map <- function(df, areas, national_areas) {

  df <- df %>%
    filter(age_group != "15-24") %>%
    #' Assuming the survey_id is structured as ISO2000DHS
    mutate(year = {if("year" %in% names(.)) year else substr(survey_id, 4, 7)}) %>%
    #' Only the most recent survey in each year
    group_by(iso3) %>%
    filter(year == max(year)) %>%
    ungroup() %>%
    left_join(
      select(areas, area_id, geometry),
      by = "area_id"
    ) %>%
    st_as_sf()

  priority_iso3 <- multi.utils::priority_iso3()

  df_subnational <- filter(df, !(area_id %in% priority_iso3))
  df_national <- setdiff(df, df_subnational)

  #' Using these just to show missing data for the countries we don't consider in the analysis
  national_areas <- national_areas %>%
    #' These are just chosen manually by looking at countries between CMR and the rest on a map
    filter(GID_0 %in% c("AGO", "DRC", "CAF", "COD", "COG", "GAB", "GNQ", "RWA", "BDI")
    ) %>%
    #' Weird but OK
    rename(iso3 = NAME_0) %>%
    select(-GID_0)

  df_national_areas <- crossing(
    indicator = unique(df_national$indicator),
    age_group = unique(df_national$age_group),
    iso3 = unique(national_areas$iso3)
  ) %>%
    left_join(national_areas, by = "iso3")

  df_subnational <- bind_rows(
    df_subnational,
    df_national_areas
  )

  ggplot(df_subnational, aes(fill = estimate_smoothed)) +
    geom_sf(size = 0.1, colour = scales::alpha("grey", 0.25)) +
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
      legend.key.width = unit(4, "lines"),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 9)
    )
}

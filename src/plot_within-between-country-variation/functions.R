prepare_estimates <- function(df) {
  df <- df %>%
    filter(age_group != "15-24") %>%
    mutate(
      #' Assuming the survey_id is structured as ISO2000DHS
      year = substr(survey_id, 4, 7),
    ) %>%
    #' Only the most recent survey in each year
    group_by(iso3) %>%
    filter(year == max(year)) %>%
    ungroup()

  df_subnational <- df %>%
    filter(!(area_id %in% multi.utils::priority_iso3()))

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

  return(list(df_subnational = df_subnational, df_national = df_national))
}

prepare_estimates <- function(df) {
  df <- df %>%
    filter(
      age_group != "15-24",
      year == 2018
    )

  df_subnational <- df

  df_national <- df %>%
    group_by(iso3, age_group, indicator) %>%
    summarise(
      estimate_smoothed = sum(estimate_smoothed * population_mean, na.rm = TRUE) / sum(population_mean, na.rm = TRUE)
    )

  #' Countries that are missing a national level aggregate
  missing_national <- df_national %>%
    filter(is.na(estimate_smoothed)) %>%
    pull(iso3) %>%
    unique() %>%
    length()

  missing_national == 0

  #' Add region column
  #' Defined based on the UN geoscheme for Africa
  #' https://en.wikipedia.org/wiki/United_Nations_geoscheme_for_Africa
  region_key <- c(
    "Botswana" = "Southern",
    "Cameroon" = "Central",
    "Kenya" = "Eastern",
    "Lesotho" = "Southern",
    "Mozambique" = "Eastern",
    "Malawi" = "Eastern",
    "Namibia" = "Southern",
    "Eswatini" = "Southern",
    "Tanzania" = "Eastern",
    "Uganda" = "Eastern",
    "South Africa" = "Southern",
    "Zambia" = "Eastern",
    "Zimbabwe" = "Eastern"
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

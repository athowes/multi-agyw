differentiate_high_risk <- function(df, df_prop) {
  df_sexnonreg <- df %>%
    filter(indicator == "sexnonregplus") %>%
    left_join(
      df_prop %>%
        select(age_group, area_id, estimate_smoothed_prop = estimate_smoothed, estimate_raw_prop = estimate_raw),
      by = c("age_group", "area_id")
    ) %>%
    mutate(
      indicator = "sexnonreg",
      estimate_smoothed = estimate_smoothed * (1 - estimate_smoothed_prop),
      estimate_part_raw = estimate_raw * (1 - estimate_smoothed_prop),
      estimate_raw = estimate_raw * (1 - estimate_raw_prop)
    ) %>%
    select(-estimate_smoothed_prop, -estimate_raw_prop)

  df_sexpaid12m <- df %>%
    filter(indicator == "sexnonregplus") %>%
    left_join(
      df_prop %>%
        select(age_group, area_id, estimate_smoothed_prop = estimate_smoothed, estimate_raw_prop = estimate_raw),
      by = c("age_group", "area_id")
    ) %>%
    mutate(
      indicator = "sexpaid12m",
      estimate_smoothed = estimate_smoothed * estimate_smoothed_prop,
      estimate_part_raw = estimate_raw * estimate_smoothed_prop,
      estimate_raw = estimate_raw * estimate_raw_prop
    ) %>%
    select(-estimate_smoothed_prop, -estimate_raw_prop)

  #' Check that each of the categories have the same number of rows
  stopifnot(nrow(df_sexnonreg) == nrow(df_sexpaid12m))
  stopifnot(nrow(df_sexpaid12m) == nrow(filter(df, indicator != "sexnonregplus")) / 2)

  bind_rows(
    #' The part raw estimate here is just the same as the raw estimate
    filter(df, indicator != "sexnonregplus") %>%
      mutate(estimate_part_raw = estimate_raw),
    df_sexnonreg,
    df_sexpaid12m
  )
}

#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("process_johnston-adjustment")
# setwd("src/process_johnston-adjustment")

df <- read_csv("depends/best-3p1-multi-sexbehav-sae.csv")
johnston <- read_csv("depends/johnston-fsw-comparison.csv")

#' Some NAs in here: sad reacts
johnston <- johnston %>%
  mutate(age_group = fct_relevel(age_group, "Y015_019" = "15-19", "Y020_024" = "20-24")) %>%
  select(iso3, age_group, method, est_proportion) %>%
  filter(method %in% c("Smoothed", "Johnston")) %>%
  pivot_wider(
    values_from = est_proportion,
    names_from = method
  ) %>%
  #' Such that estimate * ratio will scale the estimates to match Johnston at national level
  mutate(johnston_fsw_ratio = Johnston / Smoothed) %>%
  select(-Johnston, - Smoothed)

#' Johnston doesn't include 25-29, so let's just assume it's the same ratio as in 20-24 for now
johnston_Y025_029 <- johnston %>%
  filter(age_group == "20-24") %>%
  mutate(age_group = "25-29")

johnston <- bind_rows(johnston, johnston_Y025_029)

df <- df %>%
  left_join(
    johnston,
    by = c("iso3", "age_group")
  ) %>%
  select(iso3, indicator, survey_id, age_group, area_id, area_name, estimate_smoothed, johnston_fsw_ratio) %>%
  pivot_wider(
    names_from = indicator,
    values_from = estimate_smoothed
  ) %>%
  mutate(
    sexpaid12m = ifelse(!is.na(johnston_fsw_ratio), sexpaid12m * johnston_fsw_ratio, sexpaid12m),
    #' Rather than increasing by the amount that is lost from sexpaid12m, can just use the fact that must sum to 1
    sexnonreg = 1 - nosex12m - sexcohab - sexpaid12m
  ) %>%
  select(-johnston_fsw_ratio) %>%
  pivot_longer(
    cols = nosex12m:sexpaid12m,
    names_to = "indicator",
    values_to = "estimate_smoothed"
  )

write_csv(df, "adjust-best-3p1-multi-sexbehav-sae.csv")
write_csv(update_naming(df), "human-adjust-best-3p1-multi-sexbehav-sae.csv")

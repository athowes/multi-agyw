#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("process_differentiate-high-risk")
# setwd("src/process_differentiate-high-risk")

df <- read_csv("depends/best-3-multinomial-smoothed-district-sexbehav.csv")

#' Just the most recent surveys (this isn't a good solution)
prop <- read_csv("depends/best-fsw-logit-smoothed-district-sexbehav.csv") %>%
  filter(!(survey_id %in% c("MWI2015DHS", "ZMB2016PHIA", "ZWE2015DHS")))

z <- df %>%
  filter(indicator == "sexnonregplus") %>%
  left_join(
    prop %>%
      select(age_group, area_id, estimate_smoothed_prop = estimate_smoothed, estimate_raw_prop = estimate_raw),
    by = c("age_group", "area_id")
  )

df_sexnonreg <- z %>%
  mutate(
    indicator = "sexnonreg",
    estimate_smoothed = estimate_smoothed * (1 - estimate_smoothed_prop),
    estimate_raw = estimate_raw * (1 - estimate_raw_prop)
  ) %>%
  select(-estimate_smoothed_prop, -estimate_raw_prop)

df_sexpaid12m <- z %>%
  mutate(
    indicator = "sexpaid12m",
    estimate_smoothed = estimate_smoothed * estimate_smoothed_prop,
    estimate_raw = estimate_raw * estimate_raw_prop
  ) %>%
  select(-estimate_smoothed_prop, -estimate_raw_prop)

#' Check that each of the categories have the same number of rows
stopifnot(nrow(df_sexnonreg) == nrow(df_sexpaid12m))
stopifnot(nrow(df_sexpaid12m) == nrow(filter(df, indicator != "sexnonregplus")) / 2)

df <- bind_rows(
  filter(df, indicator != "sexnonregplus"),
  df_sexnonreg,
  df_sexpaid12m
)

write_csv(df, "best-3p1-multinomial-smoothed-district-sexbehav.csv", na = "")
write_csv(multi.utils::update_naming(df), "human-best-3p1-multinomial-smoothed-district-sexbehav.csv", na = "")

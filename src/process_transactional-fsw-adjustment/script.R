#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("process_transactional-fsw-adjustment")
# setwd("src/process_transactional-fsw-adjustment")

#' Could add a new indicator name like ywkp or fsw rather than sexpaid12m
#' This would help clarify that the estimate is now different
df_3p1 <- read_csv("depends/best-3p1-multi-sexbehav-sae.csv")
fsw <- read_csv("depends/oli-johnston-fsw-comparison.csv")

fsw <- fsw %>%
  mutate(transactional_fsw_ratio = total_oli / total_smoothed) %>%
  select(age_group, iso3 = area_id, transactional_fsw_ratio) %>%
  mutate(age_group = fct_recode(age_group, "Y015_019" = "15-19", "Y020_024" = "20-24", "Y025_029" = "25-29"))

df_3p1_adjusted <- df_3p1 %>%
  left_join(
    fsw,
    by = c("iso3", "age_group")
  ) %>%
  select(iso3, year, indicator, survey_id, age_group, area_id, area_name, estimate_smoothed, transactional_fsw_ratio) %>%
  pivot_wider(
    names_from = indicator,
    values_from = estimate_smoothed
  ) %>%
  mutate(
    sexpaid12m = ifelse(
      is.finite(transactional_fsw_ratio) & !is.na(transactional_fsw_ratio),
      sexpaid12m * transactional_fsw_ratio, sexpaid12m
    ),
    #' Rather than increasing by the amount that is lost from sexpaid12m, can just use the fact that must sum to 1
    sexnonreg = 1 - nosex12m - sexcohab - sexpaid12m
  ) %>%
  select(-transactional_fsw_ratio) %>%
  pivot_longer(
    cols = nosex12m:sexpaid12m,
    names_to = "indicator",
    values_to = "estimate_smoothed"
  )

write_csv(df_3p1_adjusted, "adjust-best-3p1-multi-sexbehav-sae.csv")

pdf("adjustment-comparison.pdf", h = 5, w = 6.25)

bind_rows(
  df_3p1 %>%
    filter(indicator == "sexpaid12m", year == 2018) %>%
    mutate(adjustment = "Unadjusted") %>%
    select(age_group, area_id, estimate_smoothed, adjustment),
  df_3p1_adjusted %>%
    filter(indicator == "sexpaid12m", year == 2018) %>%
    mutate(adjustment = "Adjusted") %>%
    select(age_group, area_id, estimate_smoothed, adjustment)
) %>%
  pivot_wider(
    names_from = adjustment,
    values_from = estimate_smoothed
  ) %>%
  mutate(iso3 = substr(area_id, 1, 3)) %>%
  ggplot(aes(x = Unadjusted, y = Adjusted, col = iso3, shape = age_group)) +
  geom_point(alpha = 0.5) +
  lims(x = c(0, 0.1), y = c(0, 0.1)) +
  theme_minimal()

dev.off()

#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("process_naomi-data")
# setwd("src/process_naomi-data")

priority_iso3 <- multi.utils::priority_iso3()
analysis_level <- multi.utils::analysis_level()

naomi_extract <- readRDS("naomi_extract.rds")

naomi <- naomi_extract %>%
  filter(sex=="female") %>%
  select(
    iso3, area_id, area_level, age_group = age_group_label,
    indicator = indicator_label, estimate = mean
  )

saveRDS(naomi, "naomi.rds")

pop <- naomi %>%
  filter(indicator == "Population") %>%
  select(-indicator) %>%
  rename(population = estimate) %>%
  pivot_wider(
    names_from = age_group,
    values_from = population
  ) %>%
  mutate(
    `25-49` = `25-29` + `30-34` + `35-39` + `40-44` + `45-49`,
    `15-29` = `15-19` + `20-24` + `25-29`,
  ) %>%
  pivot_longer(
    cols = -c(iso3, area_id, area_level),
    names_to = "age_group",
    values_to = "population"
  )

saveRDS(pop, "naomi_pop.rds")

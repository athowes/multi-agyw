#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("process_all-data")
# setwd("src/process_all-data")

iso3 <- multi.utils::priority_iso3()

#' Merge all of the area datasets
areas <- lapply(iso3, function(x) read_sf(paste0("depends/", tolower(x), "_areas.geojson")))
areas[[9]]$epp_level <- as.numeric(areas[[9]]$epp_level) #' Fix non-conforming column type
areas <- bind_rows(areas)

pdf("areas.pdf", h = 11, w = 6.25)
plot(areas$geometry)
dev.off()

saveRDS(areas, "areas.rds")

#' Merge all of the indicator datasets
ind <- lapply(iso3, function(x) read_csv(paste0("depends/", tolower(x), "_survey_indicators_sexbehav.csv"))) %>%
  bind_rows()

write_csv(ind, "survey_indicators_sexbehav.csv")

pdf("survey_indicators_sexbehav.pdf", h = 10, w = 6.25)

ind %>%
  filter(indicator %in% c("nosex12m", "sexcohab", "sexnonregplus")) %>%
  group_by(indicator, survey_id) %>%
  summarise(estimate = mean(estimate)) %>%
  mutate(
    iso3 = substr(survey_id, 1, 3),
    year = as.numeric(substr(survey_id, 4, 7)),
    type = substr(survey_id, 8, 11)
  ) %>%
  ggplot(aes(x = year, y = estimate, col = type)) +
  geom_point() +
  facet_grid(iso3 ~ indicator)

dev.off()

#' Merge all of the HIV datasets
hiv <- lapply(iso3, function(x) read_csv(paste0("depends/", tolower(x), "_hiv_indicators_sexbehav.csv"))) %>%
  bind_rows()

write_csv(hiv, "hiv_indicators_sexbehav.csv")

#' Merge all of the population datasets
pop <- lapply(iso3, function(x) read_csv(paste0("depends/", tolower(x), "_interpolated-population.csv"))) %>%
  bind_rows()

write_csv(pop, "interpolated_population.csv")

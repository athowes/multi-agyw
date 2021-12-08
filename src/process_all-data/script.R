#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("process_all-data")
# setwd("src/process_all-data")

iso3 <- c("BWA", "CMR", "KEN", "LSO", "MOZ", "MWI", "NAM", "SWZ", "TZA", "UGA", "ZAF", "ZMB", "ZWE")

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

#' Merge all of the population datasets
pop <- lapply(iso3, function(x) read_csv(paste0("depends/", tolower(x), "_interpolated-population.csv"))) %>%
  bind_rows()

write_csv(pop, "interpolated_population.csv")

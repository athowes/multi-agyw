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
indicators <- lapply(iso3, function(x) read_csv(paste0("depends/", tolower(x), "_survey_indicators_sexbehav.csv"))) %>%
  bind_rows()

write_csv(indicators, "survey_indicators_sexbehav.csv")

#' Merge all of the population datasets

#' Dirty temporary solution because orderly isn't working here
# archives <- list.dirs("../../archive/aaa_scale_pop", recursive = FALSE)
# files <- paste0(archives, "/interpolated_population.csv")
# population <- lapply(files, function(file) read_csv(file)) %>%
#   bind_rows()
# write_csv(population, "../../global/interpolated_population_all.csv")

population <- read_csv("depends/interpolated_population_all.csv")
write_csv(population, "interpolated_population.csv")

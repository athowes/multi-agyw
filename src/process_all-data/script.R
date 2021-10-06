#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("process_all-data")
# setwd("src/process_all-data")

iso3 <- c("BWA", "CMR", "KEN", "LSO", "MOZ", "MWI", "NAM", "SWZ", "TZA", "UGA", "ZAF", "ZMB", "ZWE")

analysis_level <- c("BWA" = 2,
                    "CMR" = 2,
                    "KEN" = 2,
                    "LSO" = 1,
                    "MOZ" = 2,
                    "MWI" = 5,
                    "NAM" = 2,
                    "SWZ" = 1,
                    "TZA" = 3,
                    "UGA" = 3,
                    "ZAF" = 2,
                    "ZMB" = 2,
                    "ZWE" = 2)

#' Merge all of the areas
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

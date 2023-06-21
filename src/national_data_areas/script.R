#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("national_data_areas")
# setwd("src/national_data_areas")

#' All of the iso3 codes for countries in SSA (don't need this now)
ssa <- rdhs::dhs_countries() %>%
  dplyr::filter(RegionName == "Sub-Saharan Africa") %>%
  dplyr::select(iso3 = ISO3_CountryCode) %>%
  dplyr::filter(iso3 != "") #' Remove any blanks

#' A function to download the shapefile from GADM
gadm_download <- function(admin = 1, countries, file_path) {
  urls <- paste0("https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_", countries, "_", admin, "_sf.rds")
  dests <- paste0(file_path, "gadm36_", countries, "_", admin, "_sf.rds")
  for (i in 1:length(urls)) {
    download.file(url = urls[i], dests[i])
  }
}

# dir.create("temp")
# gadm_download(admin = 0, countries = ssa$iso3, file_path = "temp/")
#
# sf <- lapply(list.files("temp", full.names = TRUE), readRDS) %>%
#   bind_rows()

priority_iso3 <- multi.utils::priority_iso3()
missing_iso3 <- c("AGO", "CAF", "COD", "COG", "GAB", "GNQ", "RWA", "BDI")
iso3 <- c(priority_iso3, missing_iso3)

sf <- lapply(iso3, function(x) {
  sf <- read_sf(paste0("depends/", tolower(x), "_areas.geojson")) %>%
    filter(area_level == 0) %>%
    mutate_at(vars(one_of('epp_level')), as.numeric)
})  %>%
  bind_rows()

#' Check that it looks like (southern and eastern) sub-Saharan Africa
pdf("national-areas.pdf", h = 8.5, w = 6.25)

plot(sf$geometry)

dev.off()

saveRDS(sf, "national_areas.rds")

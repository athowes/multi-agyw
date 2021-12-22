#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("national_data_areas")
# setwd("src/national_data_areas")

ssa <- rdhs::dhs_countries() %>%
  dplyr::filter(RegionName == "Sub-Saharan Africa") %>%
  dplyr::select(iso3 = ISO3_CountryCode) %>%
  dplyr::filter(iso3 != "") #' Remove any blanks

gadm_download <- function(admin = 1, countries, file_path) {
  urls <- paste0("https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_", countries, "_", admin, "_sf.rds")
  dests <- paste0(file_path, "gadm36_", countries, "_", admin, "_sf.rds")
  for (i in 1:length(urls)) {
    download.file(url = urls[i], dests[i])
  }
}

dir.create("temp")
gadm_download(admin = 0, countries = ssa$iso3, file_path = "temp/")

sf <- lapply(list.files("temp", full.names = TRUE), readRDS) %>%
  bind_rows()

#' Check that it looks like sub-Saharan Africa
pdf("national-areas.pdf", h = 11.75, w = 8.25)

plot(sf$geometry)

dev.off()

saveRDS(sf, "national_areas.rds")

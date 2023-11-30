#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("process_areas")
# setwd("src/process_areas")

priority_iso3 <- multi.utils::priority_iso3()
analysis_level <- multi.utils::analysis_level()

#' Read in areas file
areas <- read_sf("2023_ssa_all_levels.geojson") %>%
  st_make_valid()

# center_coords <- do.call(rbind, st_centroid(areas)$geometry)
# areas <- areas %>%
#   mutate(center_x = center_coords[,1],
#          center_y = center_coords[,2])

areas <- areas %>%
  mutate(center = sf::st_point_on_surface(geometry),
         center_x = sf::st_coordinates(center)[,1],
         center_y = sf::st_coordinates(center)[,2]) %>%
  select(-center)

for(i in unique(areas$iso3)) {
  tempdat <- areas %>%
    filter(iso3==i)
  sf::st_write(tempdat,tolower(paste0(i,"_areas.geojson")))
}

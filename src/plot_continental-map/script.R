#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("plot_continental-map")
# setwd("src/plot_continental-map")

df_3 <- read_csv("depends/human-best-3-multinomial-smoothed-district-sexbehav.csv")
df_3p1 <- read_csv("depends/human-best-3p1-multinomial-smoothed-district-sexbehav.csv")
areas <- readRDS("depends/areas.rds")
national_areas <- readRDS("depends/national_areas.rds")

pdf("3-continental-map.pdf", h = 8, w = 6.25)

continental_map(df_3, areas, national_areas)

dev.off()

pdf("3p1-continental-map.pdf", h = 8, w = 6.75)

df_3p1 %>%
  mutate(iso3 = substr(survey_id, 1, 3)) %>%
  continental_map(areas, national_areas)

dev.off()

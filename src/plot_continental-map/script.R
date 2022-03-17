#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("plot_continental-map")
# setwd("src/plot_continental-map")

df_3_aaa <- read_csv("depends/human-best-3-multi-sexbehav-sae.csv")
df_3p1_aaa <- read_csv("depends/human-best-3p1-multi-sexbehav-sae.csv")
df_3 <- read_csv("depends/multi-sexbehav-sae.csv") %>%
  mutate(iso3 = substr(area_id, 1, 3)) %>%
  multi.utils::update_naming() %>%
  filter(year == 2020)

areas <- readRDS("depends/areas.rds")
national_areas <- readRDS("depends/national_areas.rds")

pdf("3-aaa-continental-map.pdf", h = 8, w = 6.25)

continental_map(df_3, areas, national_areas)

dev.off()

pdf("3p1-aaa-continental-map.pdf", h = 8, w = 6.75)

df_3p1 %>%
  mutate(iso3 = substr(survey_id, 1, 3)) %>%
  continental_map(areas, national_areas)

dev.off()

pdf("3-continental-map.pdf", h = 8, w = 6.75)

df_3 %>%
  continental_map(areas, national_areas)

dev.off()

library(multi.utils)
run_commit_push("tza_survey_phia")
run_commit_push("tza_survey_behav")
run_commit_push("moz_survey_behav")

id <- orderly::orderly_search("latest(parameter:version == 2022)", "tza_data_areas")
tza_data_areas <- read_sf(paste0("archive/tza_data_areas/", id, "/tza_areas.geojson"))
tza_data_areas %>% filter(area_level == 4)

id <- orderly::orderly_latest("process_all-data")
areas <- readRDS(paste0("archive/process_all-data/", id, "/areas.rds"))
pop <- readRDS(paste0("archive/process_all-data/", id, "/naomi_pop.rds"))

pdf("pop-join-test.pdf", h = 11.25, w = 8.25)

pop %>%
  filter(
    area_level != 0,
    age_group == "15-49"
  ) %>%
  left_join(
    areas,
    by = "area_id"
  ) %>%
  st_as_sf() %>%
  ggplot(aes(fill = population)) +
    geom_sf() +
    scale_fill_viridis_c(option = "C")

dev.off()

pop %>%
  filter(age_group == "15-49") %>%
  group_by(iso3, area_level) %>%
  summarise(n = n()) %>%
  filter(n > 0, area_level > 0) %>%
  View()

pop %>%
  filter(
    area_level != 0,
    age_group == "15-49"
  ) %>%
  left_join(
    areas,
    by = "area_id"
  ) %>%
  filter(is.na(geometry))

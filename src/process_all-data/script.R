#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("process_all-data")
# setwd("src/process_all-data")

priority_iso3 <- multi.utils::priority_iso3()
analysis_level <- multi.utils::analysis_level()

#' Merge all of the area datasets
areas <- lapply(priority_iso3, function(x) read_sf(paste0("depends/", tolower(x), "_areas.geojson")))
areas[[9]]$epp_level <- as.numeric(areas[[9]]$epp_level) #' Fix non-conforming column type
areas <- bind_rows(areas)

pdf("areas.pdf", h = 11, w = 6.25)
plot(areas$geometry)
dev.off()

#' Check on the number of areas in each country
areas %>%
  st_drop_geometry() %>%
  select(area_id, area_level) %>%
  mutate(iso3 = substr(area_id, 1, 3)) %>%
  left_join(
    as.data.frame(analysis_level) %>%
      tibble::rownames_to_column("iso3"),
    by = "iso3"
  ) %>%
  filter(area_level == analysis_level) %>%
  group_by(iso3) %>%
  summarise(n = n())

saveRDS(areas, "areas.rds")

#' Merge all of the indicator datasets
ind <- lapply(priority_iso3, function(x) read_csv(paste0("depends/", tolower(x), "_survey_indicators_sexbehav.csv"))) %>%
  bind_rows()

write_csv(ind, "survey_indicators_sexbehav.csv")

pdf("survey_indicators_sexbehav.pdf", h = 10, w = 6.25)

ind %>%
  filter(indicator %in% c("nosex12m", "sexcohab", "sexnonregplus", "sexcohabspouse", "sexnonregspouseplus")) %>%
  group_by(indicator, survey_id) %>%
  summarise(estimate = mean(estimate)) %>%
  mutate(
    iso3 = substr(survey_id, 1, 3),
    year = as.numeric(substr(survey_id, 4, 7)),
    type = substr(survey_id, 8, 11)
  ) %>%
  ggplot(aes(x = year, y = estimate, col = type)) +
  geom_point() +
  facet_grid(iso3 ~ indicator) +
  theme_minimal()

dev.off()

pdf("spouse-comparison.pdf", h = 5, w = 4)

ind %>%
  filter(
    area_id %in% priority_iso3,
    indicator %in% c("sexcohab", "sexcohabspouse", "sexnonreg", "sexnonregspouse")
  ) %>%
  mutate(
    spouse_indicator = grepl("spouse", indicator, fixed = TRUE)
  ) %>%
  split(.$area_id) %>%
  lapply(function(x)
    ggplot(x, aes(x = spouse_indicator, y = estimate, group = spouse_indicator, fill = indicator)) +
    geom_bar(stat = "identity", alpha = 0.8, width = 0.5) +
    facet_grid(age_group ~ survey_id, space = "free_x", scales = "free_x", switch = "x") +
    scale_color_manual(values = multi.utils::cbpalette()) +
    lims(y = c(0, 1)) +
    labs(x = "") +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      plot.title = element_text(face = "bold"),
      legend.position = "bottom",
      strip.placement = "outside"
    )
  )

dev.off()

pdf("spouse-livesaway.pdf", h = 4, w = 6.25)

spouselivesaway <- ind %>%
  filter(
    area_id %in% priority_iso3,
    indicator %in% c("sexcohab", "sexcohabspouse", "sexnonreg", "sexnonregspouse"),
    age_group == "Y015_024"
  ) %>%
  pivot_wider(
    names_from = indicator,
    id_cols = c("survey_id", "area_id", "age_group"),
    values_from = estimate
  ) %>%
  select(survey_id, area_id, age_group, sexcohab:sexnonregspouse) %>%
  mutate(
    spouselivesaway = sexcohabspouse - sexcohab,
    spouselivesaway2 = sexnonreg - sexnonregspouse
  )

ggplot(spouselivesaway, aes(x = survey_id, y = spouselivesaway, col = area_id)) +
  geom_point() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = -90, hjust = 0))

ggplot(spouselivesaway, aes(x = spouselivesaway, y = spouselivesaway2, col = area_id)) +
  geom_point() +
  lims(x = c(0, 0.2), y = c(0, 0.2)) +
  theme_minimal()

dev.off()

pdf("mwi-dhs-phia-comparison.pdf", h = 6, w = 6.25)

ind %>%
  filter(
    survey_id %in% c("MWI2015DHS", "MWI2016PHIA"),
    indicator %in% c("sexcohab", "sexnonreg")
  ) %>%
  pivot_wider(
    names_from = c("indicator", "survey_id"),
    id_cols = c("area_id", "age_group"),
    values_from = estimate
  ) %>%
  mutate(
    diff_sexcohab = sexcohab_MWI2015DHS - sexcohab_MWI2016PHIA,
    diff_sexnonreg = sexnonreg_MWI2015DHS - sexnonreg_MWI2016PHIA
  ) %>%
  left_join(
    select(areas, area_id, geometry),
    by = "area_id"
  ) %>%
  st_as_sf() %>%
  select(area_id, age_group, starts_with("diff")) %>%
  pivot_longer(
    cols = starts_with("diff_"),
    names_to = "indicator",
    values_to = "estimate"
  ) %>%
  ggplot(aes(fill = estimate)) +
    geom_sf() +
    facet_grid(indicator ~ age_group) +
    scale_fill_viridis_c() +
    labs(title = "MWI2015DHS - MWI2016PHIA") +
    theme_minimal()

dev.off()

#' Merge all of the HIV datasets
hiv <- lapply(priority_iso3, function(x) read_csv(paste0("depends/", tolower(x), "_hiv_indicators_sexbehav.csv"))) %>%
  bind_rows()

write_csv(hiv, "hiv_indicators_sexbehav.csv")

#' Merge all of the population datasets (aaa_scale_pop reports from Oli's fertility repo)
pop <- lapply(priority_iso3, function(x) read_csv(paste0("depends/", tolower(x), "_interpolated-population.csv"))) %>%
  bind_rows()

write_csv(pop, "interpolated_population.csv")

#' Population option 2: Naomi population data from Sharepoint
#' Get age-stratified population total sizes from Naomi model outputs
sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")

url <- "sites/HIVInferenceGroup-WP/Shared Documents/Data/Spectrum files/2021 naomi/Naomi datasets in R/naomi3.rds"
path <- sharepoint$download(URLencode(url))
naomi3 <- readRDS(path)

url <- "sites/HIVInferenceGroup-WP/Shared Documents/Data/Spectrum files/2021 naomi/areas-extract/naomi-2021-results_pooled-area-hierarchy.csv"
path <- sharepoint$download(URLencode(url))
area_hierarchy <- read_csv(path)

naomi3 <- naomi3 %>%
  filter(
    iso3 %in% priority_iso3,
    indicator_label %in% c("Population", "PLHIV", "New infections"),
    #' These are the age groups we are considering, as well as those which are useful
    #' for disaggregation purposes
    age_group_label %in% c("15-19", "20-24", "25-29", "30-34", "35-39",
                           "40-44", "45-49", "15-24", "15-49"),
    #' Only female
    sex == "female"
  ) %>%
  #' The most recent estimates
  group_by(iso3) %>%
  filter(calendar_quarter == max(calendar_quarter)) %>%
  ungroup() %>%
  #' Merge with area hierarchy data
  left_join(
    select(area_hierarchy, area_id, parent_area_id),
    by = "area_id"
  ) %>%
  select(
    iso3, area_id, area_level, age_group = age_group_label,
    indicator = indicator_label, estimate = mean, parent_area_id
  )

#' BWA and CMR are at one level too low
naomi3 %>%
  group_by(iso3) %>%
  summarise(area_level = unique(area_level)) %>%
  filter(area_level != 0) %>%
  t()

#' So lets aggregate them upwards here
naomi3_aggregates <- naomi3 %>%
  filter(iso3 %in% c("BWA", "CMR")) %>%
  group_by(parent_area_id, age_group, indicator) %>%
  summarise(
    iso3 = iso3,
    estimate = sum(estimate)
  ) %>%
  rename(area_id = parent_area_id) %>%
  mutate(area_level = as.numeric(substr(area_id, 5, 5))) %>%
  #' For some reason some of the rows are duplicated here
  #' Could investigate this more...
  distinct()

naomi3 <- bind_rows(naomi3, naomi3_aggregates) %>%
  split(.$iso3) %>%
  lapply(function(x) {
    #' Checking here that the area_level is correct
    filter(x, area_level == analysis_level[x$iso3[1]])
  }) %>%
  bind_rows()

naomi3_national <- naomi3 %>%
  group_by(iso3, age_group, indicator) %>%
  summarise(
    estimate = sum(estimate)
  ) %>%
  mutate(
    area_level = 0,
    area_id = iso3
  )

naomi3 <- bind_rows(naomi3, naomi3_national) %>%
  select(-parent_area_id)

moz_area_mapping <- read_csv("2022_moz_area_mapping.csv") %>%
  select(-area_name, area_id = old_area_id, new_area_id = area_id)

naomi3 <- naomi3 %>%
  left_join(
    moz_area_mapping,
    by = "area_id"
  ) %>%
  mutate(area_id = ifelse(!is.na(new_area_id), new_area_id, area_id)) %>%
  select(-new_area_id)

saveRDS(naomi3, "naomi3.rds")

pop <- naomi3 %>%
  filter(indicator == "Population") %>%
  select(-indicator) %>%
  rename(population = estimate) %>%
  pivot_wider(
    names_from = age_group,
    values_from = population
  ) %>%
  mutate(
    `25-49` = `25-29` + `30-34` + `35-39` + `40-44` + `45-49`,
    `15-29` = `15-19` + `20-24` + `25-29`,
  ) %>%
  pivot_longer(
    cols = -c(iso3, area_id, area_level),
    names_to = "age_group",
    values_to = "population"
  )

saveRDS(pop, "naomi3_pop.rds")


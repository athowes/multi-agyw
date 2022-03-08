#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("process_national-fsw-comparison")
# setwd("src/process_national-fsw-comparison/")

analysis_level <- multi.utils::analysis_level()
priority_iso3 <- multi.utils::priority_iso3()

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
    indicator_label == "Population",
    #' These are the age groups we are considering,
    age_group_label %in% c("15-19", "20-24", "25-29", "15-49"),
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
  select(iso3, area_id, area_level, age_group = age_group_label, population_mean = mean, parent_area_id)

#' The BWA and CMR population are at one level too low
naomi3_aggregates <- naomi3 %>%
  filter(iso3 %in% c("BWA", "CMR")) %>%
  group_by(parent_area_id) %>%
  summarise(
    iso3 = iso3,
    age_group = age_group,
    population_mean = sum(population_mean)
  ) %>%
  rename(area_id = parent_area_id) %>%
  mutate(area_level = as.numeric(substr(area_id, 5, 5)))

naomi3 <- bind_rows(naomi3, naomi3_aggregates) %>%
  split(.$iso3) %>%
  lapply(function(x) {
    filter(x, area_level == analysis_level[x$iso3[1]])
  }) %>%
  bind_rows() %>%
  select(-parent_area_id)

naomi3_national <- naomi3 %>%
  group_by(iso3, age_group) %>%
  summarise(population_mean = sum(population_mean))

naomi3 %>% pull(iso3) %>% unique()

#' Read in the population size estimates (PSEs) for AYKP
johnston <- read_excel("aykp_pse_july17.xlsx", sheet = "FSW", range = "A3:F187")
names(johnston) <- c("region", "country", "size_15-19", "size_20-24", "size_15-24", "size_25-49")

johnston$region %>% unique()

johnston %>% filter(region %in% c("ESA", "MENA", "WCA"))

#' iso3 codes from https://gist.github.com/tadast/8827699
country_codes <- read_csv("countries_codes_and_coordinates.csv") %>%
  select(country = Country, iso3 = `Alpha-3 code`)

national_areas <- readRDS("depends/national_areas.rds")

johnston <- johnston %>%
  left_join(country_codes) %>%
  pivot_longer(cols = contains("size"), names_prefix = "size_", names_to = "age_group", values_to = "est_total_johnston") %>%
  filter(
    region %in% c("ESA", "WCA"),
    iso3 %in% priority_iso3,
    age_group %in% c("15-19", "20-24", "25-29")
  ) %>%
  select(-region) %>%
  filter(iso3 %in% priority_iso3) %>%
  left_join(
    naomi3_national,
    by = c("age_group", "iso3")
  ) %>%
  mutate(est_proportion_johnston = est_total_johnston / population_mean)

#' Check that all of the priority ISO3 are in there
stopifnot(unique(johnston$iso3) %>% length() == 13)

pdf("johnston-fsw-data.pdf", h = 8, w = 6.25)

johnston_sf <- johnston %>%
  left_join(
    select(national_areas, "iso3" = "GID_0"),
    by = "iso3"
  ) %>%
  st_as_sf()

plotA <- johnston_sf %>%
  ggplot(aes(fill = est_total_johnston)) +
  geom_sf(size = 0.1, colour = scales::alpha("grey", 0.25)) +
  scale_fill_viridis_c(option = "C", na.value = "#E6E6E6") +
  facet_grid(~age_group) +
  labs(fill = "Total") +
  theme_minimal()

plotB <- johnston_sf %>%
  ggplot(aes(fill = est_proportion_johnston)) +
  geom_sf(size = 0.1, colour = scales::alpha("grey", 0.25)) +
  scale_fill_viridis_c(option = "C",  label = label_percent(), na.value = "#E6E6E6") +
  facet_grid(~age_group) +
  labs(fill = "Proportion") +
  theme_minimal()

cowplot::plot_grid(plotA, plotB, ncol = 1)

dev.off()

#' The proportion estimates from the sexpaid12m category of our model
est <- read_csv("depends/human-best-3p1-multi-sexbehav-sae.csv") %>%
  filter(
    indicator == "YWKP",
    age_group %in% c("15-19", "20-24", "25-29")
  ) %>%
  #' Assuming the survey_id is structured as ISO2000DHS
  mutate(year = substr(survey_id, 4, 7)) %>%
  #' Only the most recent survey in each year
  group_by(iso3) %>%
  filter(year == max(year)) %>%
  select(iso3, indicator, survey_id, age_group, area_id, estimate_raw, estimate_smoothed)

#' Calculate estimates of FSW populations by age and area using proportions from ind and PSE from naomi3
df <- est %>%
  filter(!(area_id %in% priority_iso3)) %>%
  left_join(
    naomi3 %>%
      filter(iso3 %in% priority_iso3) %>%
      select(age_group, area_id, population_mean),
    by = c("age_group", "area_id")
  ) %>%
  mutate(
    iso3 = substr(survey_id, 1, 3),
    est_raw = estimate_raw * population_mean,
    est_smoothed = estimate_smoothed * population_mean
  ) %>%
  group_by(iso3, age_group) %>%
  summarise(
    est_total_raw = sum(est_raw, na.rm = TRUE),
    est_total_smoothed = sum(est_smoothed, na.rm = TRUE),
    population_mean = sum(population_mean, na.rm = TRUE)
  ) %>%
  mutate(
    est_proportion_raw = est_total_raw / population_mean,
    est_proportion_smoothed = est_total_smoothed / population_mean
  )

pdf("johnston-fsw-comparison.pdf", h = 11.25, w = 8.75)

johnston_comparison <- johnston %>%
  left_join(
    select(df, iso3, age_group, est_proportion_raw, est_proportion_smoothed),
    by = c("iso3", "age_group")
  ) %>%
  pivot_longer(
    cols = starts_with("est_proportion"),
    names_to = "method",
    names_prefix = "est_proportion_",
    values_to = "est_proportion"
  ) %>%
  mutate(method = fct_recode(method, "Raw" = "raw", "Johnston" = "johnston", "Smoothed" = "smoothed"))

johnston_comparison %>%
  ggplot(aes(x = method, y = est_proportion, fill = method)) +
  geom_col() +
  facet_wrap(iso3 ~ age_group, scales = "free") +
  scale_fill_manual(values = multi.utils::cbpalette()) +
  labs(x = "", y = "", fill = "") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank()
  )

johnston_comparison %>%
  left_join(
    select(national_areas, "iso3" = "GID_0"),
    by = "iso3"
  ) %>%
  st_as_sf() %>%
  ggplot(aes(fill = est_proportion)) +
  geom_sf(size = 0.1, colour = scales::alpha("grey", 0.25)) +
  scale_fill_viridis_c(option = "C",  label = label_percent(), na.value = "#E6E6E6") +
  facet_grid(method ~ age_group) +
  labs(fill = "Proportion") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank()
  )

dev.off()

pdf("johnston-fsw-comparison-xy.pdf", h = 5, w = 6.25)

johnston %>%
  left_join(
    select(df, iso3, age_group, est_proportion_raw, est_proportion_smoothed),
    by = c("iso3", "age_group")
  ) %>%
  ggplot(aes(x = est_proportion_johnston, y = est_proportion_smoothed, col = iso3)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  facet_grid(age_group ~ .) +
  lims(x = c(0, 0.1), y = c(0, 0.1)) +
  labs(x = "Johnston estimate", y = "Smoothed estimate", col = "ISO3") +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )

dev.off()

write_csv(johnston_comparison, "johnston-fsw-comparison.csv", na = "")

#' Look at the Laga estimates
laga <- read_csv("final_country_est_laga.csv")

laga <- laga %>%
  rename(
    iso3 = ISO,
    country = NAME_0,
    est_total_laga = Fitted_FSW
  ) %>%
  select(-X, -ref_pop, -Uncertainty, -Upper, -Lower, -Prev, -Percent, -Predictor_only, -Predictor_only_rank) %>%
  left_join(
    naomi3_national %>%
      group_by(iso3) %>%
      summarise(population_mean = sum(population_mean)),
    by = "iso3"
  )

laga_comparison <- df %>%
  group_by(iso3) %>%
  summarise(
    est_total_raw = sum(est_total_raw),
    est_total_smoothed = sum(est_total_smoothed)
  ) %>%
  left_join(select(laga, iso3, est_total_laga, population_mean), by = "iso3") %>%
  mutate(
    est_proportion_raw = est_total_raw / population_mean,
    est_proportion_smoothed = est_total_smoothed / population_mean,
    est_proportion_laga = est_total_laga / population_mean
  ) %>%
  pivot_longer(
    cols = starts_with("est_proportion"),
    names_to = "method",
    names_prefix = "est_proportion_",
    values_to = "est_proportion"
  ) %>%
  mutate(method = fct_recode(method, "Raw" = "raw", "Laga" = "laga", "Smoothed" = "smoothed"))

write_csv(laga_comparison, "laga-fsw-comparison.csv", na = "")

pdf("laga-fsw-comparison.pdf", h = 8, w = 8.75)

laga_comparison %>%
  ggplot(aes(x = method, y = est_proportion, fill = method)) +
  geom_col() +
  facet_wrap(iso3 ~ ., scales = "free") +
  scale_fill_manual(values = multi.utils::cbpalette()) +
  labs(x = "", y = "", fill = "") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank()
  )

laga_comparison %>%
  left_join(
    select(national_areas, "iso3" = "GID_0"),
    by = "iso3"
  ) %>%
  st_as_sf() %>%
  ggplot(aes(fill = est_proportion)) +
  geom_sf(size = 0.1, colour = scales::alpha("grey", 0.25)) +
  scale_fill_viridis_c(option = "C",  label = label_percent(), na.value = "#E6E6E6") +
  facet_grid(~ method) +
  labs(fill = "Proportion") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(4, "lines"),
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank()
  )

dev.off()

pdf("laga-fsw-comparison-xy.pdf", h = 5, w = 6.25)

df %>%
  group_by(iso3) %>%
  summarise(
    est_total_raw = sum(est_total_raw),
    est_total_smoothed = sum(est_total_smoothed)
  ) %>%
  left_join(select(laga, iso3, est_total_laga, population_mean), by = "iso3") %>%
  mutate(
    est_proportion_raw = est_total_raw / population_mean,
    est_proportion_smoothed = est_total_smoothed / population_mean,
    est_proportion_laga = est_total_laga / population_mean
  ) %>%
  ggplot(aes(x = est_proportion_laga, y = est_proportion_smoothed, col = iso3)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  lims(x = c(0, 0.1), y = c(0, 0.1)) +
  labs(x = "Laga estimate", y = "Smoothed estimate", col = "ISO3") +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )

dev.off()

#' Look at the Oli's estimates
oli <- read_csv("pse_estimates.csv") %>%
  select(-"...1") %>%
  filter(
    kp == "FSW",
    iso3 %in% priority_iso3
  )

pdf("oli-fsw-data.pdf", h = 8, w = 6.25)

oli_sf <- oli %>%
  left_join(
    select(national_areas, "iso3" = "GID_0"),
    by = "iso3"
  ) %>%
  st_as_sf()

plotA <- oli_sf %>%
  ggplot(aes(fill = median)) +
  geom_sf(size = 0.1, colour = scales::alpha("grey", 0.25)) +
  scale_fill_viridis_c(option = "C", na.value = "#E6E6E6") +
  labs(fill = "Median proportion") +
  theme_minimal()

plotB <- oli_sf %>%
  ggplot(aes(fill = median)) +
  geom_sf(size = 0.1, colour = scales::alpha("grey", 0.25)) +
  scale_fill_viridis_c(option = "C",  label = label_percent(), na.value = "#E6E6E6") +
  labs(fill = "Median proportion") +
  theme_minimal()

cowplot::plot_grid(plotA, plotB, ncol = 1)

dev.off()

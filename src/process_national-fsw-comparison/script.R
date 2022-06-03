#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("process_national-fsw-comparison")
# setwd("src/process_national-fsw-comparison/")

#' All available FSW data sources
#'
#' * Paper: "Deriving and interpreting population size estimates for adolescent
#'   and young key populations at higher risk of HIV transmission: men who have
#'   sex with men, female sex workers and transgender persons"
#' * First author: Lisa Johnston
#' * Areas: National
#' * Age groups: 15-19, 20-24, 25-49
#' * Code: Similar to process_age-disagg-fsw
#'
#' * Paper: "Estimating key population size, HIV prevalence, and ART coverage
#'   for sub-Saharan Africa at the national level"
#' * First author: Oliver Stevens
#' * Areas: National
#' * Age groups: 15-49
#' * Code: https://github.com/osymandius/kp-data
#' * Notes: I've age disaggregated these estimates to five year age bands using
#'   the same method as Johnston et al. in the report process_age-disagg-fsw
#'
#' * Paper: "Mapping female sex worker prevalence (aged 15-49 years) in
#'   sub-Saharan Africa"
#' * First author: Ian Laga
#' * Areas: Sub-national
#' * Age-groups: 15-49
#' * Code: https://github.com/ilaga/Mapping-FSW-SSA

analysis_level <- multi.utils::analysis_level()
priority_iso3 <- multi.utils::priority_iso3()

pop <- readRDS("depends/naomi_pop.rds")

#' Read in the population size estimates (PSEs) for AYKP
johnston <- read_excel("aykp_pse_july17.xlsx", sheet = "FSW", range = "A3:F187")
names(johnston) <- c("region", "country", "size_15-19", "size_20-24", "size_15-24", "size_25-49")

#' iso3 codes from https://gist.github.com/tadast/8827699
country_codes <- read_csv("countries_codes_and_coordinates.csv") %>%
  select(country = Country, iso3 = `Alpha-3 code`)

national_areas <- readRDS("depends/national_areas.rds")

johnston <- johnston %>%
  left_join(country_codes) %>%
  mutate(`size_15-49` = `size_15-19` + `size_20-24` + `size_25-49`) %>%
  pivot_longer(
    cols = contains("size"),
    names_prefix = "size_",
    names_to = "age_group",
    values_to = "total"
  ) %>%
  filter(
    region %in% c("ESA", "WCA"),
    iso3 %in% priority_iso3,
    age_group %in% c("15-19", "20-24", "25-49", "15-49")
  ) %>%
  select(-region) %>%
  left_join(
    pop %>%
      filter(area_id %in% priority_iso3),
    by = c("age_group", "iso3")
  ) %>%
  mutate(proportion = total / population)

#' Check that all of the priority ISO3 are in there
stopifnot(unique(johnston$iso3) %>% length() == 13)

pdf("johnston-fsw-data.pdf", h = 4, w = 6.25)

johnston_sf <- johnston %>%
  pivot_longer(
    cols = c(total, proportion),
    names_to = "indicator",
    values_to = "estimate"
  ) %>%
  left_join(
    select(national_areas, "iso3" = "area_id"),
    by = "iso3"
  ) %>%
  st_as_sf()

johnston_sf %>%
  filter(indicator == "total") %>%
  ggplot(aes(fill = estimate)) +
  geom_sf(size = 0.1, colour = scales::alpha("grey", 0.25)) +
  scale_fill_viridis_c(option = "C", na.value = "#E6E6E6") +
  facet_wrap(~age_group) +
  labs(title = "Johnston total FSW estimate", fill = "Total") +
  theme_void()

johnston_sf %>%
  filter(indicator == "proportion") %>%
  ggplot(aes(fill = estimate)) +
  geom_sf(size = 0.1, colour = scales::alpha("grey", 0.25)) +
  scale_fill_viridis_c(option = "C", na.value = "#E6E6E6") +
  facet_wrap(~age_group) +
  labs(title = "Johnston FSW proportion estimate", fill = "Proportion") +
  theme_void()

dev.off()

#' The proportion estimates from the sexpaid12m category of our model
#' These are pre-adjustment and therefore correspond to transactional sex
est <- read_csv("depends/best-3p1-multi-sexbehav-sae.csv") %>%
  multi.utils::update_naming() %>%
  filter(
    indicator == "FSW",
    age_group %in% c("15-19", "20-24", "25-29")
  ) %>%
  filter(year == 2018) %>%
  select(iso3, indicator, survey_id, age_group, area_id, estimate_raw, estimate_smoothed)

#' Calculate estimates of FSW populations by age and area using
#' * proportions from ind
#' * population data from pop
df <- est %>%
  left_join(
    pop %>%
      select(age_group, area_id, population),
    by = c("age_group", "area_id")
  ) %>%
  mutate(
    iso3 = substr(area_id, 1, 3),
    raw = estimate_raw * population,
    smoothed = estimate_smoothed * population
  ) %>%
  group_by(iso3, age_group) %>%
  summarise(
    total_raw = sum(raw, na.rm = TRUE),
    total_smoothed = sum(smoothed, na.rm = TRUE),
    population = sum(population, na.rm = TRUE)
  ) %>%
  mutate(
    proportion_raw = total_raw / population,
    proportion_smoothed = total_smoothed / population
  )

pdf("johnston-fsw-comparison.pdf", h = 11.25, w = 8.75)

johnston_comparison <- johnston %>%
  rename(total_johnston = total, proportion_johnston = proportion) %>%
  left_join(
    select(df, iso3, age_group, proportion_raw, proportion_smoothed),
    by = c("iso3", "age_group")
  ) %>%
  pivot_longer(
    cols = starts_with("proportion"),
    names_to = "method",
    names_prefix = "proportion_",
    values_to = "proportion"
  ) %>%
  mutate(method = fct_recode(method, "Raw" = "raw", "Johnston" = "johnston", "Smoothed" = "smoothed"))

johnston_comparison %>%
  left_join(
    select(national_areas, "iso3" = "area_id"),
    by = "iso3"
  ) %>%
  st_as_sf() %>%
  ggplot(aes(fill = proportion)) +
  geom_sf(size = 0.1, colour = scales::alpha("grey", 0.25)) +
  scale_fill_viridis_c(option = "C",  label = label_percent(), na.value = "#E6E6E6") +
  facet_grid(method ~ age_group) +
  labs(fill = "Proportion") +
  theme_void() +
  theme(
    legend.position = "bottom",
  )

dev.off()

pdf("johnston-fsw-comparison-xy.pdf", h = 5, w = 6.25)

johnston %>%
  rename(total_johnston = total, proportion_johnston = proportion) %>%
  left_join(
    select(df, iso3, age_group, proportion_raw, proportion_smoothed),
    by = c("iso3", "age_group")
  ) %>%
  ggplot(aes(x = proportion_johnston, y = proportion_smoothed, col = iso3)) +
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
    total_laga = Fitted_FSW
  ) %>%
  select(-X, -ref_pop, -Uncertainty, -Upper, -Lower, -Prev, -Percent, -Predictor_only, -Predictor_only_rank) %>%
  mutate(age_group = "15-49") %>%
  filter(iso3 %in% priority_iso3) %>%
  left_join(
    pop %>%
      filter(area_id %in% priority_iso3),
    by = c("iso3", "age_group")
  )

laga_comparison <- df %>%
  group_by(iso3) %>%
  summarise(
    total_raw = sum(total_raw),
    total_smoothed = sum(total_smoothed)
  ) %>%
  left_join(
    select(laga, iso3, total_laga, population),
    by = "iso3"
  ) %>%
  mutate(
    proportion_raw = total_raw / population,
    proportion_smoothed = total_smoothed / population,
    proportion_laga = total_laga / population
  ) %>%
  pivot_longer(
    cols = starts_with("proportion"),
    names_to = "method",
    names_prefix = "proportion_",
    values_to = "proportion"
  ) %>%
  mutate(method = fct_recode(method, "Raw" = "raw", "Laga" = "laga", "Smoothed" = "smoothed"))

write_csv(laga_comparison, "laga-fsw-comparison.csv", na = "")

pdf("laga-fsw-comparison.pdf", h = 8, w = 8.75)

laga_comparison %>%
  left_join(
    select(national_areas, "iso3" = "area_id"),
    by = "iso3"
  ) %>%
  st_as_sf() %>%
  ggplot(aes(fill = proportion)) +
  geom_sf(size = 0.1, colour = scales::alpha("grey", 0.25)) +
  scale_fill_viridis_c(option = "C",  label = label_percent(), na.value = "#E6E6E6") +
  facet_grid(~ method) +
  labs(fill = "Proportion") +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(4, "lines")
  )

dev.off()

pdf("laga-fsw-comparison-xy.pdf", h = 5, w = 6.25)

df %>%
  group_by(iso3) %>%
  summarise(
    total_smoothed = sum(total_smoothed)
  ) %>%
  left_join(select(laga, iso3, total_laga, population), by = "iso3") %>%
  mutate(
    proportion_smoothed = total_smoothed / population,
    proportion_laga = total_laga / population
  ) %>%
  ggplot(aes(x = proportion_laga, y = proportion_smoothed, col = iso3)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  lims(x = c(0, 0.1), y = c(0, 0.1)) +
  labs(x = "Laga proportion estimate (15-49)", y = "Smoothed proportion estimate (15-29)", col = "ISO3") +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )

dev.off()

#' Look at Oli's estimates
oli <- read_csv("pse_estimates.csv") %>%
  select(-"...1") %>%
  filter(
    kp == "FSW",
    iso3 %in% priority_iso3
  )

pdf("oli-fsw-data.pdf", h = 8, w = 6.25)

oli_sf <- oli %>%
  left_join(
    select(national_areas, "iso3" = "area_id"),
    by = "iso3"
  ) %>%
  st_as_sf()

oli_sf %>%
  ggplot(aes(fill = median)) +
  geom_sf(size = 0.1, colour = scales::alpha("grey", 0.25)) +
  scale_fill_viridis_c(option = "C",  label = label_percent(), na.value = "#E6E6E6") +
  labs(fill = "Median proportion") +
  theme_minimal()

dev.off()

#' Import age-disaggregated version of Oli's FSW data
oli_age <- read_csv("depends/fsw-estimates.csv")

oli_johnston_comparison <- johnston %>%
  select(-country, -iso3, -population, - proportion) %>%
  rename(total_johnston = total) %>%
  left_join(
    select(df, area_id = iso3, age_group, total_raw, total_smoothed),
    by = c("area_id", "age_group")
  ) %>%
  left_join(
    oli_age %>%
      select(area_id, age_group, fsw) %>%
      rename(total_oli = fsw) %>%
      mutate(
        age_group = recode_factor(
          age_group,
          "Y015_019" = "15-19", "Y020_024" = "20-24",
          "Y025_029" = "25-29", "Y015_049" = "15-49"
        )
      ),
    by = c("area_id", "age_group")
  )

pdf("oli-johnston-fsw-comparison-xy.pdf", h = 5, w = 6.25)

oli_johnston_comparison %>%
  filter(age_group == "15-49") %>%
  filter(!is.na(total_johnston), !is.na(total_oli)) %>%
  ggplot(aes(x = total_johnston, y = total_oli, col = area_id, shape = age_group)) +
  geom_point(alpha = 0.8, size = 2) +
  lims(x = c(0, 5 * 10^5), y = c(0, 5 * 10^5)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", col = "grey") +
  theme_minimal() +
  labs(x = "Total FSW (Johnston)", y = "Total FSW (Oli)")

oli_johnston_comparison %>%
  filter(age_group != "15-49") %>%
  filter(!is.na(total_johnston), !is.na(total_oli)) %>%
  ggplot(aes(x = total_johnston, y = total_oli, col = area_id, shape = age_group)) +
  geom_point(alpha = 0.8, size = 2) +
  lims(x = c(0, 10^5), y = c(0, 10^5)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", col = "grey") +
  theme_minimal() +
  labs(x = "FSW (Johnston)", y = "FSW (Oli)")

dev.off()

write_csv(oli_johnston_comparison, "oli-johnston-fsw-comparison.csv")

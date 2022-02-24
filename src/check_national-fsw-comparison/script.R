#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("check_national-fsw-comparison")
# setwd("src/check_national-fsw-comparison/")

analysis_level <- multi.utils::analysis_level()
priority_iso3 <- multi.utils::priority_iso3()

#' Read in the population size estimates (PSEs) for AYKP
johnston <- read_excel("aykp_pse_july17.xlsx", sheet = "FSW", range = "A3:F187")
names(johnston) <- c("region", "country", "size_15-19", "size_20-24", "size_15-24", "size_25-49")

#' iso3 codes from https://gist.github.com/tadast/8827699
country_codes <- read_csv("countries_codes_and_coordinates.csv") %>%
  select(country = Country, iso3 = `Alpha-3 code`)

johnston <- johnston %>%
  left_join(country_codes) %>%
  pivot_longer(cols = contains("size"), names_prefix = "size_", names_to = "age_group", values_to = "est_total_johnston") %>%
  filter(
    region %in% c("ESA", "WCA"),
    iso3 %in% priority_iso3,
    age_group %in% c("15-19", "20-24", "25-29")
  ) %>%
  select(-region)

#' Johnston is missing estimates for SWZ
priority_iso3[!(priority_iso3 %in% johnston$iso3)]

#' Look at the Laga estimates
laga <- read_csv("final_country_est_laga.csv")

laga <- laga %>%
  rename(
    iso3 = ISO,
    country = NAME_0,
    est_total_laga = Fitted_FSW
  ) %>%
  select(-X, -ref_pop, -Uncertainty, -Upper, -Lower, -Prev, -Percent, -Predictor_only, -Predictor_only_rank)

#' These are the proportion estimates from the sexpaid12m category of our model
ind <- read_csv("depends/human-best-3p1-multinomial-smoothed-district-sexbehav.csv") %>%
  mutate(iso3 = substr(survey_id, 1, 3))

ind <- ind %>%
  filter(indicator == "YWKP") %>%
  filter(age_group %in% c("15-19", "20-24", "25-29"))

#' Get age-stratified population total sizes from Naomi model outputs
sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")
url <- "sites/HIVInferenceGroup-WP/Shared Documents/Data/Spectrum files/2021 naomi/Naomi datasets in R/naomi3.rds"
path <- sharepoint$download(URLencode(url))
naomi3 <- readRDS(path)

naomi3 <- naomi3 %>%
  filter(
    indicator_label == "Population",
    #' The most recent estimates
    calendar_quarter == max(calendar_quarter),
    #' These are the age groups we are considering,
    age_group_label %in% c("15-19", "20-24", "25-29"),
    #' Only female
    sex == "female"
  ) %>%
  split(.$iso3) %>%
  #' Filtering each country to the relevant analysis level
  #' Maybe this can be done with map2
  lapply(function(x)
    x %>%
      filter(area_level == analysis_level[x$iso3[1]])
  ) %>%
  bind_rows() %>%
  #' For merging with model data
  rename(
    population_mean = mean,
    population_lower = lower,
    population_upper = upper
  ) %>%
  select(-indicator_label)

#' Calculate estimates of FSW populations by age and area using proportions from ind and PSE from naomi3
df <- ind %>%
  select(-population_mean) %>%
  inner_join(naomi3, by = c("age_group" = "age_group_label", "area_name" = "area_name", "iso3" = "iso3")) %>%
  #' Note that the names of estimate and mean will change once data is rerun!
  #' Making an iso3 variable will also not be required
  mutate(
    est_raw = round(estimate_raw * population_mean, digits = 3),
    est_smoothed = round(estimate_smoothed * population_mean, digits = 3)
  ) %>%
  group_by(iso3, age_group) %>%
  summarise(
    est_total_raw = sum(est_raw, na.rm = TRUE),
    est_total_smoothed = sum(est_smoothed, na.rm = TRUE)
  )

#' Comparison to Johnston
johnston_comparison <- df %>%
  inner_join(johnston, by = c("iso3", "age_group")) %>%
  relocate(country, .before = iso3)

write_csv(johnston_comparison, "johnston-fsw-comparison.csv", na = "")

pdf("johnston-fsw-comparison.pdf", h = 7, w = 6.25)

johnston_comparison %>%
  pivot_longer(
    cols = starts_with("est_total"),
    names_to = "method",
    names_prefix = "est_total_",
    values_to = "est_total"
  ) %>%
  mutate(method = fct_recode(method, "Raw" = "raw", "Johnston" = "johnston", "Smoothed" = "smoothed")) %>%
  ggplot(aes(x = method, y = est_total, fill = method)) +
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

dev.off()

#' Comparison to Laga
laga_comparison <- df %>%
  group_by(iso3) %>%
  summarise(
    est_total_raw = sum(est_total_raw),
    est_total_smoothed = sum(est_total_smoothed)
  ) %>%
  inner_join(laga, by = c("iso3")) %>%
  relocate(country, .before = iso3)

write_csv(laga_comparison, "laga-fsw-comparison.csv", na = "")

laga_comparison %>%
  select(-est_total_raw) %>%
  pivot_longer(
    cols = c(est_total_smoothed, est_total_laga),
    names_to = "method",
    names_prefix = "est_total_",
    values_to = "est_total"
  ) %>%
  ggplot(aes(x = iso3, y = est_total, col = method)) +
  geom_point()

pdf("laga-fsw-comparison.pdf", h = 7, w = 6.25)

laga_comparison %>%
  pivot_longer(
    cols = starts_with("est_total"),
    names_to = "method",
    names_prefix = "est_total_",
    values_to = "est_total"
  ) %>%
  mutate(method = fct_recode(method, "Raw" = "raw", "Laga" = "laga", "Smoothed" = "smoothed")) %>%
  ggplot(aes(x = method, y = est_total, fill = method)) +
  geom_col() +
  facet_wrap(~iso3, scales = "free") +
  scale_fill_manual(values = multi.utils::cbpalette()) +
  labs(x = "", y = "", fill = "") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank()
  )

dev.off()

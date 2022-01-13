#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("process_katies-spreadsheet")
# setwd("src/process_katies-spreadsheet")

analysis_level <- c("BWA" = 2,
                    "CMR" = 2,
                    "KEN" = 2,
                    "LSO" = 1,
                    "MOZ" = 2,
                    "MWI" = 5,
                    "NAM" = 2,
                    "SWZ" = 1,
                    "TZA" = 3,
                    "UGA" = 3,
                    "ZAF" = 2,
                    "ZMB" = 2,
                    "ZWE" = 2)

df <- read_csv("depends/best-3-multinomial-smoothed-district-sexbehav.csv")
areas <- readRDS("depends/areas.rds")

#' Confusing file names here, but I believe they are all the same thing
files <- c(
  "bwa_20210329-142306_naomi_spectrum_digest",
  "CMR_20210427-155423_naomi_spectrum_digest",
  "ken-naomi-output_calibrated-sex-age-coarse_2021-03-30",
  "LSO_naomi-output_20210616-0905",
  "MOZ_20210504-101529_naomi_spectrum_digest",
  "MWI_20210428-213633_sex-age-coarse-calibrated_naomi_spectrum_digest",
  "nam_20210408-003934_naomi_spectrum_digest",
  "SWZ_naomi-output_20210616-1231",
  "tza-naomi-output-time2-art_calibrated-sex-age-coarse_2021-04-13",
  "uga-2016-art-adjusted_naomi-output_calibrated-sex-age-fine_2021-02-05",
  "zaf_district_naomi-output_calibrated-province",
  "zmb-naomi-output_calibrated-sex-age-coarse_2021-04-27",
  "zwe_20210331-161516_naomi_spectrum_digest"
)

naomi <- lapply(files, function(file) read.csv(paste0("naomi-output/", file, "/indicators.csv"))) %>%
  bind_rows()

naomi <- naomi %>%
  mutate(
    iso3 = substr(area_id, 1, 3),
    analysis_level = analysis_level[iso3]
  ) %>%
  filter(
    area_level == analysis_level,
    age_group %in% c("Y015_019", "Y020_024", "Y025_029", "Y015_024"),
    indicator %in% c("infections", "plhiv", "population"),
    sex == "female",
    case_when(
      iso3 %in% c("TZA", "ZAF") ~ calendar_quarter == "CY2020Q3",
      TRUE ~ calendar_quarter == "CY2020Q4"
    )
  ) %>%
  select(iso3, area_id, sex, age_group, indicator, mean) %>%
  pivot_wider(
    names_from = indicator,
    values_from = mean
  ) %>%
  mutate(
    #' In terms of new infections per hundred person years
    incidence = 100 * infections / (population - plhiv),
    incidence_cat = cut(
      incidence,
      c(0, 0.1, 0.3, 1, 10^6),
      labels = c("Low", "Moderate", "High", "Very High"),
      include.lowest = TRUE,
      right = TRUE
    )
  ) %>%
  #' Only female, so don't need this column
  select(-sex)

df <- df %>%
  mutate(year = substr(survey_id, 4, 7)) %>%
  #' Only the most recent survey in each year
  group_by(iso3) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  select(area_id, age_group, indicator, estimate_smoothed) %>%
  pivot_wider(
    names_from = indicator,
    values_from = estimate_smoothed,
    values_fn = mean
  )

df <- naomi %>%
  left_join(
    df,
    by = c("area_id", "age_group")
  )

rr_sexcohab <- 1
rr_sexnonreg <- 1.72 #' Note that this is wrong because I've grouped FSW into here!

df <- df %>%
  mutate(
    population_nosex12m = population * nosex12m,
    population_sexcohab = population * sexcohab,
    population_sexnonregplus = population * sexnonregplus,
    incidence_nosex12m = 0,
    incidence_sexcohab = infections / (population_sexcohab + 1.72 * population_sexnonregplus),
    incidence_sexnonregplus = incidence_sexcohab * 1.72,
    infections_nosex12m = 0,
    infections_sexcohab = population_sexcohab * incidence_sexcohab,
    infections_sexnonregplus = population_sexnonregplus * incidence_sexnonregplus
  )

write_csv(df, "incidence-district-sexbehav.csv")

df_plot <- df %>%
  select(iso3, area_id, age_group, starts_with("incidence_sex")) %>%
  pivot_longer(
    cols = starts_with("incidence_sex"),
    names_to = "category",
    names_prefix = "incidence_",
    values_to = "incidence",
  ) %>%
  left_join(
    select(areas, area_id),
    by = "area_id"
  ) %>%
  st_as_sf()

#' Artefact: Cloropleths

pdf("incidence-district-sexbehav.pdf", h = 8.25, w = 11.75)

df_plot %>%
  split(.$iso3) %>%
  lapply(function(x)
    x %>%
      mutate(
        age_group = fct_relevel(age_group, "Y015_024") %>%
          fct_recode(
            "15-19" = "Y015_019",
            "20-24" = "Y020_024",
            "25-29" = "Y025_029",
            "15-24" = "Y015_024"
          )
      ) %>%
      ggplot(aes(fill = incidence)) +
      geom_sf(size = 0.1) +
      scale_fill_viridis_c(option = "C", label = label_percent()) +
      facet_grid(category ~ age_group) +
      theme_minimal() +
      labs(
        title = paste0(x$iso3[1]),
        fill = "Incidence rate"
      ) +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        legend.position = "bottom",
        legend.key.width = unit(4, "lines")
      )
  )

dev.off()

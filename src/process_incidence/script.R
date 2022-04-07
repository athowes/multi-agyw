#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("process_incidence")
# setwd("src/process_incidence")

analysis_level <- multi.utils::analysis_level()

df_3p1 <- read_csv("depends/best-3p1-multi-sexbehav-sae.csv")
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
    age_group %in% c("Y015_019", "Y020_024", "Y025_029"),
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
      c(0, 0.1, 0.3, 1, 3, 10^6),
      labels = c("Low", "Moderate", "High", "Very High", "Very Very High"),
      include.lowest = TRUE,
      right = TRUE
    )
  ) %>%
  #' Only female, so don't need this column
  select(-sex)

df_3p1 <- df_3p1 %>%
  filter(year == 2018) %>%
  select(area_id, age_group, indicator, estimate_smoothed) %>%
  pivot_wider(
    names_from = indicator,
    values_from = estimate_smoothed,
    values_fn = mean
  )

df_3p1 <- naomi %>%
  left_join(
    df_3p1,
    by = c("area_id", "age_group")
  )

rr_sexcohab <- 1
rr_sexnonreg <- 1.72

#' Tiered HIV risk ratio for the FSW group depending on district-level HIV incidence in general population
rr_sexpaid12m_vvh <- 3 #' >3%
rr_sexpaid12m_vh <- 6 #' 1-3%
rr_sexpaid12m_h <- 9 #' 0.3-1%
rr_sexpaid12m_m <- 13 #' 0.1-0.3%
rr_sexpaid12m_l <- 25 #' <0.1%

#' TODO: Get distributions on these and using a sampling method to get uncertainty in economic analysis e.g.
rr_sexnonreg_se <- 0.2
rr_sexnonreg_se <- 1

df_3p1 <- df_3p1 %>%
  mutate(
    rr_sexpaid12m = case_when(
      incidence_cat == "Very Very High" ~ rr_sexpaid12m_vvh,
      incidence_cat == "Very High" ~ rr_sexpaid12m_vh,
      incidence_cat == "High" ~ rr_sexpaid12m_h,
      incidence_cat == "Moderate" ~ rr_sexpaid12m_m,
      incidence_cat == "Low" ~ rr_sexpaid12m_l
    ),
    population_nosex12m = population * nosex12m,
    population_sexcohab = population * sexcohab,
    population_sexnonreg = population * sexnonreg,
    population_sexpaid12m = population * sexpaid12m,
    incidence_nosex12m = 0,
    incidence_sexcohab = infections / (population_sexcohab + rr_sexnonreg * population_sexnonreg + rr_sexpaid12m * population_sexpaid12m),
    incidence_sexnonreg = incidence_sexcohab * rr_sexnonreg,
    incidence_sexpaid12m = incidence_sexcohab * rr_sexpaid12m,
    infections_nosex12m = 0,
    infections_sexcohab = population_sexcohab * incidence_sexcohab,
    infections_sexnonreg = population_sexnonreg * incidence_sexnonreg,
    infections_sexpaid12m = population_sexpaid12m * incidence_sexpaid12m
  )

write_csv(df_3p1, "incidence-district-sexbehav.csv")

df_3p1_plot <- df_3p1 %>%
  select(iso3, area_id, age_group, starts_with("incidence_sex")) %>%
  pivot_longer(
    cols = starts_with("incidence_sex"),
    names_to = "indicator",
    names_prefix = "incidence_",
    values_to = "incidence",
  ) %>%
  left_join(
    select(areas, area_id),
    by = "area_id"
  ) %>%
  st_as_sf()

#' Artefact: Cloropleths
pdf("incidence-district-sexbehav.pdf", h = 8, w = 6.25)

plotsA <- df_3p1_plot %>%
  multi.utils::update_naming() %>%
  split(.$iso3) %>%
  lapply(function(x)
    x %>%
      ggplot(aes(fill = incidence)) +
      geom_sf(size = 0.1, colour = scales::alpha("grey", 0.25)) +
      coord_sf(lims_method = "geometry_bbox") +
      scale_fill_viridis_c(option = "C", label = label_percent()) +
      facet_grid(age_group ~ indicator, labeller = labeller(indicator = label_wrap_gen(10))) +
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
        legend.position = "bottom",
        legend.key.width = unit(4, "lines")
      )
  )

plotsA

dev.off()

#' Sadly multi-page .png don't exist
#' This is a bit clunky but unsure if there is a better option
lapply(1:length(plotsA), function(i) {
  ggsave(
    paste0("incidence-district-sexbehav-", i, ".png"),
    plotsA[[i]],
    width = 6.25, height = 8, units = "in", dpi = 300
  )
})

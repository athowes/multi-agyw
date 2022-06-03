#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("process_incidence")
# setwd("src/process_incidence")

analysis_level <- multi.utils::analysis_level()

df_3p1 <- read_csv("depends/adjust-best-3p1-multi-sexbehav-sae.csv")
prev <- read_csv("depends/prev-district-sexbehav-logit.csv")
naomi <- readRDS("depends/naomi.rds")
areas <- readRDS("depends/areas.rds")

naomi <- naomi %>%
  pivot_wider(
    names_from = indicator,
    values_from = estimate
  ) %>%
  rename(infections = `New infections`, plhiv = PLHIV, population = Population) %>%
  mutate(
    age_group = fct_recode(age_group,
      "Y015_019" = "15-19",
      "Y020_024" = "20-24",
      "Y025_029" = "25-29",
      "Y015_024" = "15-24",
      "Y015_049" = "15-49"
    ),
    #' In terms of new infections per hundred person years
    incidence = 100 * infections / (population - plhiv),
    incidence_cat = cut(
      incidence,
      c(0, 0.1, 0.3, 1, 3, 10^6),
      labels = c("Low", "Moderate", "High", "Very High", "Very Very High"),
      include.lowest = TRUE,
      right = TRUE
    )
  )

df_3p1 <- df_3p1 %>%
  filter(year == 2018) %>%
  select(area_id, age_group, indicator, estimate_smoothed) %>%
  pivot_wider(
    names_from = indicator,
    values_from = estimate_smoothed,
    values_fn = mean
  )

df <- df_3p1 %>%
  left_join(
    naomi,
    by = c("area_id", "age_group")
  ) %>%
  left_join(
    select(prev, "area_id", "age_group", starts_with("prev_")),
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

df <- df %>%
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
    plhiv_nosex12m = population_nosex12m * prev_nosex12m,
    plhiv_sexcohab = population_sexcohab * prev_sexcohab,
    plhiv_sexnonreg = population_sexnonreg * prev_sexnonreg,
    plhiv_sexpaid12m = population_sexpaid12m * prev_sexpaid12m,
    incidence_nosex12m = 0,
    incidence_sexcohab = infections / (population_sexcohab +
      rr_sexnonreg * population_sexnonreg + rr_sexpaid12m * population_sexpaid12m),
    incidence_sexnonreg = incidence_sexcohab * rr_sexnonreg,
    incidence_sexpaid12m = incidence_sexcohab * rr_sexpaid12m,
    infections_nosex12m = 0,
    infections_sexcohab = (population_sexcohab - plhiv_sexcohab) * incidence_sexcohab,
    infections_sexnonreg = (population_sexnonreg - plhiv_sexnonreg) * incidence_sexnonreg,
    infections_sexpaid12m = (population_sexpaid12m - plhiv_sexpaid12m) * incidence_sexpaid12m
  )

write_csv(df, "incidence-district-sexbehav.csv")

df_plot <- df %>%
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

plotsA <- df_plot %>%
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

pdf("infections-district-sexbehav.pdf", h = 8, w = 6.25)

df %>%
  select(iso3, area_id, age_group, starts_with("infections_sex")) %>%
  pivot_longer(
    cols = starts_with("infections_sex"),
    names_to = "indicator",
    names_prefix = "infections_",
    values_to = "infections",
  ) %>%
  left_join(
    select(areas, area_id),
    by = "area_id"
  ) %>%
  st_as_sf() %>%
  multi.utils::update_naming() %>%
  split(.$iso3) %>%
  lapply(function(x)
    x %>%
      ggplot(aes(fill = infections)) +
      geom_sf(size = 0.1, colour = scales::alpha("grey", 0.25)) +
      coord_sf(lims_method = "geometry_bbox") +
      scale_fill_viridis_c(option = "C") +
      facet_grid(age_group ~ indicator, labeller = labeller(indicator = label_wrap_gen(10))) +
      theme_minimal() +
      labs(
        title = paste0(x$iso3[1]),
        fill = "New infections"
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

dev.off()

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
  ) %>%
  #' Cabo Delgado Province in MOZ disrupted due to conflict, so no population data from Naomi
  filter(!is.na(population))

# df %>% filter(is.na(population)) %>%
#   pull(area_id) %>%
#   unique()

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
    susceptible_nosex12m = population_nosex12m - plhiv_nosex12m,
    susceptible_sexcohab = population_sexcohab - plhiv_sexcohab,
    susceptible_sexnonreg = population_sexnonreg - plhiv_sexnonreg,
    susceptible_sexpaid12m = population_sexpaid12m - plhiv_sexpaid12m,
    incidence_nosex12m = 0,
    incidence_sexcohab = infections / (susceptible_sexcohab +
      rr_sexnonreg * susceptible_sexnonreg + rr_sexpaid12m * susceptible_sexpaid12m),
    incidence_sexnonreg = incidence_sexcohab * rr_sexnonreg,
    incidence_sexpaid12m = incidence_sexcohab * rr_sexpaid12m,
    infections_nosex12m = 0,
    infections_sexcohab = susceptible_sexcohab * incidence_sexcohab,
    infections_sexnonreg = susceptible_sexnonreg * incidence_sexnonreg,
    infections_sexpaid12m = susceptible_sexpaid12m * incidence_sexpaid12m
  )

#' Check that sum of disaggregated infections is the same as total infections
sum_infections <- df$infections_nosex12m + df$infections_sexcohab + df$infections_sexnonreg + df$infections_sexpaid12m
stopifnot(max(df$infections - sum_infections) < 10^{-9})

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
pdf("incidence-district-sexbehav.pdf", h = 7, w = 6.25)

plotsA <- df_plot %>%
  multi.utils::update_naming() %>%
  split(.$iso3) %>%
  lapply(function(x)
    x %>%
      ggplot(aes(fill = incidence)) +
      geom_sf(size = 0.1, colour = scales::alpha("grey", 0.25)) +
      coord_sf(lims_method = "geometry_bbox") +
      scale_fill_viridis_c(option = "C", label = label_percent()) +
      facet_grid(age_group ~ indicator, labeller = labeller(indicator = label_wrap_gen(20))) +
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

pdf("infections-district-sexbehav.pdf", h = 7, w = 6.25)

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
      facet_grid(age_group ~ indicator, labeller = labeller(indicator = label_wrap_gen(20))) +
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

pdf("map-ian.pdf", h = 6, w = 6.25)

df_ian <- df %>%
  filter(age_group %in% c("Y015_019", "Y020_024")) %>%
  group_by(area_id) %>%
  summarise(
    infections_sexcohab = sum(infections_sexcohab),
    infections_sexnonreg = sum(infections_sexnonreg),
    infections_sexpaid12m = sum(infections_sexpaid12m),
    incidence_sexcohab = 100 * infections_sexcohab / sum(susceptible_sexcohab),
    incidence_sexnonreg = 100 * infections_sexnonreg / sum(susceptible_sexnonreg),
    incidence_sexpaid12m = 100 * infections_sexpaid12m / sum(susceptible_sexpaid12m),
  ) %>%
  pivot_longer(
    cols = c(starts_with("incidence_"), starts_with("infections")),
    names_to = "indicator",
    values_to = "estimate",
  ) %>%
  separate(indicator, into = c("indicator", "behav")) %>%
  mutate(age_group = "Y015_024")

naomi_aggregate <- naomi %>%
  filter(age_group == "Y015_024") %>%
  select(area_id, age_group, incidence, infections) %>%
  pivot_longer(
    cols = c("incidence", "infections"),
    values_to = "estimate",
    names_to = "indicator"
  ) %>%
  mutate(behav = "all")

df_ian <- bind_rows(df_ian, naomi_aggregate)

df_ian_sf <- df_ian %>%
  mutate(iso3 = substr(area_id, 1, 3)) %>%
  left_join(
    select(areas, area_id),
    by = "area_id"
  ) %>%
  st_as_sf()

df_ian_sf %>%
  mutate(
    behav = fct_recode(behav,
      "All" = "all",
      "Cohabiting" = "sexcohab",
      "Non-regular partner(s)" = "sexnonreg",
      "YWKPs" = "sexpaid12m")
  ) %>%
  filter(indicator == "incidence") %>%
  ggplot(aes(fill = estimate)) +
  geom_sf(size = 0.1, colour = scales::alpha("grey", 0.25)) +
  coord_sf(lims_method = "geometry_bbox") +
  scale_fill_gradient2(
    midpoint = 1, limits = c(0, 3), oob = scales::squish,
    breaks = c(0, 1, 3), labels=c("0", "1", "3+"),
    low = viridis::plasma(3, direction = -1)[1],
    mid = viridis::plasma(3, direction = -1)[2],
    high = viridis::plasma(3, direction = -1)[3]
  ) +
  # scale_fill_viridis_c(option = "C", limits = c(0, 3), oob = scales::squish) +
  facet_wrap(~behav, labeller = labeller(indicator = label_wrap_gen(10)), nrow = 2) +
  labs(fill = "Incidence rate \n(per 100 PYAR)") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "right"
  )

dev.off()

saveRDS(df_ian_sf, "data-ian.rds")

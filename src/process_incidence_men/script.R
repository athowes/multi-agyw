#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("process_incidence_men")
# setwd("src/process_incidence_men")

priority_iso3 <- multi.utils::priority_iso3()
analysis_level <- multi.utils::analysis_level()

df_3p1 <- read_csv("depends/adjust-best-3p1-multi-sexbehav-sae.csv")
prev <- read_csv("depends/prev-district-sexbehav-logit.csv")
naomi <- readRDS("depends/naomi.rds")
areas <- readRDS("depends/areas.rds")

# for french
naomi$indicator[naomi$indicator=="Nouvelles infections"] <- "New infections"

kps <- readRDS("kplhiv_art.rds")

kp_prev <- bind_rows(kps$MSM$area %>% filter(indicator=="prev" & iso3 %in% priority_iso3) %>%
                       mutate(kp = "MSM"),
                     kps$PWID$area %>% filter(indicator=="prev" & iso3 %in% priority_iso3) %>%
                       mutate(kp = "PWID"))

kp_prev <- kp_prev %>%
  select(-indicator,-lower, - upper) %>%
  mutate(area_level = as.numeric(substr(area_id,5,5)))

# no data for cabo delgado - fix temporarily w/ mean for country
kp_prev <- rbind(kp_prev,
                 c("MOZ","MOZ_1_10",mean(kp_prev$median[kp_prev$iso3=="MOZ" & kp_prev$kp=="MSM"]),
                   "MSM",1),
                 c("MOZ","MOZ_1_10",mean(kp_prev$median[kp_prev$iso3=="MOZ" & kp_prev$kp=="PWID"]),
                   "PWID",1))
kp_prev$median <- as.numeric(kp_prev$median)
kp_prev$area_level <- as.numeric(kp_prev$area_level)

kp_prev <- kp_prev %>%
  left_join(
    naomi %>%
      filter(age_group=="15-49") %>%
      pivot_wider(
        values_from = "estimate",
        names_from = "indicator"
      ) %>%
      mutate(gen_prev = (PLHIV / Population) ) %>%
      select(area_id, gen_prev)
  ) %>%
  select(kp, iso3, area_id, gen_prev, median, area_level)

kp_prev$iso3 <- as.character(kp_prev$iso3)

# fill something in for Haiti since not available in Oli's analysis
kp_prev <- rbind(kp_prev,
                 # data for MSM coming from 2019 KP atlas estimate + aidsinfo men's
                 # prevalence 15-49 2019 (accessed 5 feb 2023)
                 c("MSM","HTI","HTI",log(0.015/(1-0.015)),log(0.045/(1-0.045)),0),
                 # no data for PWID - currently using median PWID prevalence for
                 # areas that are within +/- 0.1 prevalence of Haiti
                 c("PWID","HTI","HTI",
                   log(0.045/(1-0.045)),
                   median(kp_prev$median[kp_prev$kp=="PWID" & kp_prev$gen_prev>=(0.15-.1) & kp_prev$gen_prev<=(0.15+.1)]),0))

kp_prev$gen_prev <- as.numeric(kp_prev$gen_prev)
kp_prev$median <- as.numeric(kp_prev$median)
kp_prev$area_level <- as.numeric(kp_prev$area_level)

### GET THE APPROPRIATE AREA TO MERGE TO KP ESTIMATES

df_3p1 <- df_3p1 %>%
  left_join(areas %>%
              select(area_id,parent_area_id)) %>%
  rename(parent1 = parent_area_id) %>%
  mutate(parent1_level = as.numeric(substr(parent1,5,5))) %>%
  left_join(areas %>%
              select(area_id,parent_area_id),
            by=c("parent1" = "area_id")) %>%
  rename(parent2 = parent_area_id) %>%
  mutate(parent2_level = as.numeric(substr(parent2,5,5))) %>%
  left_join(areas %>%
              select(area_id,parent_area_id),
            by=c("parent2" = "area_id")) %>%
  rename(parent3 = parent_area_id) %>%
  mutate(parent3_level = as.numeric(substr(parent3,5,5))) %>%
  left_join(areas %>%
              select(area_id,parent_area_id),
            by= c("parent3" = "area_id")) %>%
  rename(parent4 = parent_area_id) %>%
  mutate(parent4_level = as.numeric(substr(parent4,5,5)))

kp_level <- kp_prev %>%
  group_by(iso3) %>%
  summarise(area_level_kp = mean(area_level))

kp_analysis_level <- kp_level$area_level_kp
names(kp_analysis_level) <- kp_level$iso3

df_3p1 <- df_3p1 %>%
  left_join(
    as.data.frame(kp_analysis_level) %>%
      tibble::rownames_to_column("iso3"),
    by = "iso3"
  ) %>%
  mutate(area_level = as.numeric(substr(area_id,5,5))) %>%
  mutate(kp_match_area = case_when(kp_analysis_level == area_level ~ area_id,
                                   kp_analysis_level == parent1_level ~ parent1,
                                   kp_analysis_level == parent2_level ~ parent2,
                                   kp_analysis_level == parent3_level ~ parent3,
                                   kp_analysis_level == parent4_level ~ parent4,
                                   TRUE ~ NA_character_))

df_3p1$kp_match_area[df_3p1$iso3=="HTI"] <- "HTI"

df_3p1 <- df_3p1 %>%
  select(!(parent1:kp_analysis_level))


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
      "Y015_049" = "15-49",
      "Y025_049" = "25-49",
      "Y030_034" = "30-34",
      "Y035_039" = "35-39",
      "Y040_044" = "40-44",
      "Y045_049" = "45-49"
    ),
    #' In terms of new infections per hundred person years
    incidence = 100 * infections / (population - plhiv),
    incidence_cat = cut(
      incidence,
      c(0, 0.3, 1, 3, 10^6),
      labels = c("Low", "Moderate", "High", "Very High"),
      include.lowest = TRUE,
      right = TRUE
    )
  )

df_3p1 <- df_3p1 %>%
  filter(year == 2018) %>%
  select(area_id, kp_match_area, age_group, indicator, estimate_smoothed) %>%
  pivot_wider(
    names_from = indicator,
    values_from = estimate_smoothed,
    values_fn = mean
  )

kp_prev <- kp_prev %>%
  filter(!is.na(area_id)) %>%
  pivot_wider(names_from = kp,
              values_from = c("gen_prev","median")) %>%
  mutate(msm_pr = median_MSM / gen_prev_MSM,
         pwid_pr = median_PWID / gen_prev_PWID) %>%
  select(-c("gen_prev_PWID","gen_prev_MSM","median_PWID","median_MSM","area_level"))

df <- df_3p1 %>%
  left_join(
    select(kp_prev,-iso3),
    by = c("kp_match_area" = "area_id")
  ) %>%
  select(-kp_match_area) %>%
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
rr_sexnonreg_young <- 1.89
rr_sexnonreg_old <- 2.1

# # HIV risk ratio for PWID & MSM (Make this more nuanced in future!)
# rr_msm <- 15
# rr_pwid <- 7.8

#' TODO: Get distributions on these and using a sampling method to get uncertainty in economic analysis e.g.
rr_sexnonreg_se <- 0.2
rr_sexnonreg_se <- 1

df <- df %>%
  mutate(
    # correcting since the reference cat is reg cohabiting not gen pop
    # need more sustainable fix for this
    rr_msm = ifelse(msm_pr>2.5,msm_pr,2.5),
    rr_pwid = ifelse(pwid_pr>2.5,pwid_pr,2.5),
    rr_sexnonreg = case_when(
      age_group %in% c("Y015_019","Y020_024","Y015_024") ~ rr_sexnonreg_young,
      age_group %in% c("Y025_029","Y030_034","Y035_039","Y040_044","Y045_049",
                       "Y025_049") ~ rr_sexnonreg_old,
      TRUE ~ NA_real_
    ),
    population_nosex12m = population * nosex12m,
    population_sexcohab = population * sexcohab,
    population_sexnonreg = population * sexnonreg,
    population_msm = population * msm,
    population_pwid = population * pwid,
    plhiv_nosex12m = population_nosex12m * prev_nosex12m,
    plhiv_sexcohab = population_sexcohab * prev_sexcohab,
    plhiv_sexnonreg = population_sexnonreg * prev_sexnonreg,
    plhiv_msm = population_msm * prev_msm,
    plhiv_pwid = population_pwid * prev_pwid,
    susceptible_nosex12m = population_nosex12m - plhiv_nosex12m,
    susceptible_sexcohab = population_sexcohab - plhiv_sexcohab,
    susceptible_sexnonreg = population_sexnonreg - plhiv_sexnonreg,
    susceptible_msm = population_msm - plhiv_msm,
    susceptible_pwid = population_pwid - plhiv_pwid,
    incidence_nosex12m = 0,
    incidence_sexcohab = infections / (susceptible_sexcohab +
      rr_sexnonreg * susceptible_sexnonreg + rr_msm * susceptible_msm +
        rr_pwid * susceptible_pwid),
    incidence_sexnonreg = incidence_sexcohab * rr_sexnonreg,
    incidence_msm = incidence_sexcohab * rr_msm,
    incidence_pwid = incidence_sexcohab * rr_pwid,
    infections_nosex12m = 0,
    infections_sexcohab = susceptible_sexcohab * incidence_sexcohab,
    infections_sexnonreg = susceptible_sexnonreg * incidence_sexnonreg,
    infections_msm = susceptible_msm * incidence_msm,
    infections_pwid = susceptible_pwid * incidence_pwid
  )

#' Check that sum of disaggregated infections is the same as total infections
sum_infections <- df$infections_nosex12m + df$infections_sexcohab + df$infections_sexnonreg + df$infections_msm + df$infections_pwid
stopifnot(max(df$infections - sum_infections) < 10^{-9})

write_csv(df, "incidence-district-sexbehav.csv")

df_plot <- df %>%
  select(iso3, area_id, age_group, starts_with(c("incidence_sex", "incidence_msm", "incidence_pwid"))) %>%
  pivot_longer(
    cols = starts_with(c("incidence_sex", "incidence_msm", "incidence_pwid")),
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
  select(iso3, area_id, age_group, starts_with(c("infections_sex", "infections_msm", "infections_pwid"))) %>%
  pivot_longer(
    cols = starts_with(c("infections_sex", "infections_msm", "infections_pwid")),
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
    infections_msm = sum(infections_msm),
    infections_pwid = sum(infections_pwid),
    incidence_sexcohab = 100 * infections_sexcohab / sum(susceptible_sexcohab),
    incidence_sexnonreg = 100 * infections_sexnonreg / sum(susceptible_sexnonreg),
    incidence_msm = 100 * infections_msm / sum(susceptible_msm),
    incidence_pwid = 100 * infections_pwid / sum(susceptible_pwid)
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
      "MSM" = "msm",
      "PWID" = "pwid")
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

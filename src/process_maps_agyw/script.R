#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("process_maps_agyw")
# setwd("src/process_maps_agyw")

analysis_level <- multi.utils::analysis_level()

df <- read_csv("depends/incidence-district-sexbehav_allages.csv")
areas <- readRDS("depends/areas.rds")
naomi <- readRDS("depends/naomi.rds")

pdf("maps_incidence_agyw_all.pdf", h = 6, w = 6.25)

df_maps <- df %>%
  filter(age_group %in% c("Y015_024")) %>%
  select(iso3,area_id,incidence_nosex12m,incidence_sexcohab,incidence_sexnonreg,incidence_sexpaid12m) %>%
  pivot_longer(
    cols = c(starts_with("incidence_")),
    names_to = "indicator",
    values_to = "estimate",
  ) %>%
  separate(indicator, into = c("indicator", "behav")) %>%
  filter(behav!="nosex12m") %>%
  mutate(estimate = estimate * 100)

# for french
naomi$indicator[naomi$indicator=="Nouvelles infections"] <- "New infections"

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

naomi_aggregate <- naomi %>%
  filter(age_group == "Y015_024") %>%
  select(iso3, area_id, age_group, incidence) %>%
  pivot_longer(
    cols = c("incidence"),
    values_to = "estimate",
    names_to = "indicator"
  ) %>%
  mutate(behav = "all") %>%
  select(-age_group)

df_maps <- bind_rows(df_maps, naomi_aggregate)

fct_reorg <- function(fac, ...) {
  fct_recode(fct_relevel(fac, ...), ...)
}

df_maps <- df_maps %>%
  mutate(iso3 = recode_factor(iso3, Botswana = "BWA",
                          Cameroon = "CMR", Kenya = "KEN", Lesotho = "LSO",
                          Mozambique = "MOZ", Malawi = "MWI", Namibia = "NAM",
                          Eswatini = "SWZ", Tanzania = "TZA", Uganda = "UGA",
                          `South Africa` = "ZAF", Zambia = "ZMB", Zimbabwe = "ZWE",
                          Angola = "AGO", Burundi = "BDI", `Burkina Faso` = "BFA",
                          `Cote d'Ivoire` = "CIV", `Democratic Republic of the Congo` = "COD",
                          Ethiopia = "ETH", Gabon = "GAB", Ghana = "GHA", Guinea = "GIN",
                          Liberia = "LBR", Mali = "MLI", Niger = "NER", Rwanda = "RWA",
                          `Sierra Leone` = "SLE", Chad = "TCD", Togo = "TGO", Haiti = "HTI"))

df_maps_sf <- df_maps %>%
  left_join(
    select(areas, area_id),
    by = "area_id"
  ) %>%
  st_as_sf()

countries <- unique(df_maps_sf$iso3)

countries <- countries[countries!="Haiti"]

for(i in 1:length(countries)) {
  print(df_maps_sf %>%
    filter(iso3==countries[i]) %>%
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
    labs(fill = "Incidence rate \n(per 100 PYAR)",
         title = countries[i]) +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      strip.text = element_text(face = "bold"),
      legend.position = "right"
    ))
}

dev.off()


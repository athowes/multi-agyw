#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("process_prevalence")
# setwd("src/process_prevalence")

analysis_level <- multi.utils::analysis_level()

df_3p1 <- read_csv("depends/adjust-best-3p1-multi-sexbehav-sae.csv")
areas <- readRDS("depends/areas.rds")
prev_pr <- read_csv("katie-prev-pr.csv")
naomi3 <- readRDS("naomi3.rds")

naomi3 <- naomi3 %>%
  filter(iso3 %in% multi.utils::priority_iso3()) %>%
  mutate(analysis_level = analysis_level[iso3]) %>%
  filter(
    age_group_label %in% c("15-19", "20-24", "25-29"),
    indicator_label %in% c("PLHIV", "Population"),
    sex == "female",
    case_when(
      iso3 %in% c("TZA", "ZAF") ~ calendar_quarter == "CY2020Q3",
      TRUE ~ calendar_quarter == "CY2020Q4"
    )
  ) %>%
  select(iso3, area_id, sex, age_group_label, indicator_label, mean) %>%
  pivot_wider(
    names_from = indicator_label,
    values_from = mean
  ) %>%
  mutate(
    age_group = case_when(
      age_group_label == "15-19" ~ "Y015_019",
      age_group_label == "20-24" ~ "Y020_024",
      age_group_label == "25-29" ~ "Y025_029"
    )
  ) %>%
  rename(
    population = Population,
    plhiv = PLHIV
  )

df_3p1 <- df_3p1 %>%
  filter(year == 2018) %>%
  select(area_id, age_group, indicator, estimate_smoothed) %>%
  pivot_wider(
    names_from = indicator,
    values_from = estimate_smoothed,
    values_fn = mean
  )

df_3p1 <- naomi3 %>%
  left_join(
    df_3p1,
    by = c("area_id", "age_group")
  ) %>%
  filter(!is.na(nosex12m))

#' Just the prevalence ratios
prev_pr <- prev_pr %>%
  select(iso3, starts_with("pr_"))

df_3p1 <- df_3p1 %>%
  left_join(
    prev_pr,
    by = "iso3"
  )

df_3p1 <- df_3p1 %>%
  mutate(
    population_nosex12m = population * nosex12m,
    population_sexcohab = population * sexcohab,
    population_sexnonreg = population * sexnonreg,
    population_sexpaid12m = population * sexpaid12m,
    prev_nosex12m = plhiv / (population_nosex12m +
                             pr_sexcohab * population_sexcohab +
                             pr_sexnonreg * population_sexnonreg +
                             pr_sexpaid12m * population_sexpaid12m),
    prev_sexcohab = pr_sexcohab * prev_nosex12m,
    prev_sexnonreg = pr_sexnonreg * prev_nosex12m,
    prev_sexpaid12m = pr_sexpaid12m * prev_nosex12m,
    plhiv_nosex12m = prev_nosex12m * population_nosex12m,
    plhiv_nosex12m = prev_sexcohab * population_sexcohab,
    plhiv_nosex12m = prev_sexnonreg * population_sexnonreg,
    plhiv_nosex12m = prev_sexpaid12m * population_sexpaid12m
  )

write_csv(df_3p1, "prev-district-sexbehav.csv")

df_3p1_plot <- df_3p1 %>%
  select(iso3, area_id, age_group, starts_with("prev_")) %>%
  pivot_longer(
    cols = starts_with("prev_"),
    names_to = "indicator",
    names_prefix = "prev_",
    values_to = "prev",
  ) %>%
  left_join(
    select(areas, area_id),
    by = "area_id"
  ) %>%
  st_as_sf()

#' Artefact: Cloropleths
pdf("prev-district-sexbehav.pdf", h = 8, w = 6.25)

plotsA <- df_3p1_plot %>%
  multi.utils::update_naming() %>%
  split(.$iso3) %>%
  lapply(function(x)
    x %>%
      ggplot(aes(fill = prev)) +
      geom_sf(size = 0.1, colour = scales::alpha("grey", 0.25)) +
      coord_sf(lims_method = "geometry_bbox") +
      scale_fill_viridis_c(option = "C", label = label_percent()) +
      facet_grid(age_group ~ indicator, labeller = labeller(indicator = label_wrap_gen(10))) +
      theme_minimal() +
      labs(
        title = paste0(x$iso3[1]),
        fill = "Prevalence"
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
lapply(1:length(plotsA), function(i) {
  ggsave(
    paste0("prev-district-sexbehav-", i, ".png"),
    plotsA[[i]],
    width = 6.25, height = 8, units = "in", dpi = 300
  )
})

#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("process_agegroups")
# setwd("src/process_agegroups")

alldat <- read_csv("depends/incidence-district-sexbehav.csv")

dat <- alldat %>%
  filter(age_group %in% c("Y015_019","Y020_024")) %>%
  group_by(area_id,iso3,area_level) %>%
  summarise(age_group = "Y015_024",
            population = sum(population),
            plhiv = sum(plhiv),
            infections = sum(infections),
            population_nosex12m = sum(population_nosex12m),
            population_sexcohab = sum(population_sexcohab),
            population_sexnonreg = sum(population_sexnonreg),
            population_sexpaid12m = sum(population_sexpaid12m),
            plhiv_nosex12m = sum(plhiv_nosex12m),
            plhiv_sexcohab = sum(plhiv_sexcohab),
            plhiv_sexnonreg = sum(plhiv_sexnonreg),
            plhiv_sexpaid12m = sum(plhiv_sexpaid12m),
            susceptible_nosex12m = sum(susceptible_nosex12m),
            susceptible_sexcohab = sum(susceptible_sexcohab),
            susceptible_sexnonreg = sum(susceptible_sexnonreg),
            susceptible_sexpaid12m = sum(susceptible_sexpaid12m),
            infections_nosex12m = sum(infections_nosex12m),
            infections_sexcohab = sum(infections_sexcohab),
            infections_sexnonreg = sum(infections_sexnonreg),
            infections_sexpaid12m = sum(infections_sexpaid12m))

dat <- dat %>%
  mutate(nosex12m = susceptible_nosex12m/(population-plhiv),
         sexcohab = susceptible_sexcohab/(population-plhiv),
         sexnonregplus = sum(susceptible_sexnonreg,susceptible_sexpaid12m)/(population-plhiv),
         sexnonreg = susceptible_sexnonreg/(population-plhiv),
         sexpaid12m = susceptible_sexpaid12m/(population-plhiv),
         incidence = (infections/(population-plhiv))*100,
         incidence_cat = cut(
           incidence,
           c(0, 0.3, 1, 3, 10^6),
           labels = c("Low", "Moderate", "High", "Very High"),
           include.lowest = TRUE,
           right = TRUE
         ),
         prev_nosex12m = plhiv_nosex12m/(susceptible_nosex12m + plhiv_nosex12m),
         prev_sexcohab = plhiv_sexcohab/(susceptible_sexcohab + plhiv_sexcohab),
         prev_sexnonreg = plhiv_sexnonreg/(susceptible_sexnonreg + plhiv_sexnonreg),
         prev_sexpaid12m = plhiv_sexpaid12m/(susceptible_sexpaid12m + plhiv_sexpaid12m),
         rr_sexpaid12m = NA,
         incidence_nosex12m = infections_nosex12m/susceptible_nosex12m,
         incidence_sexcohab = infections_sexcohab/susceptible_sexcohab,
         incidence_sexnonreg = infections_sexnonreg/susceptible_sexnonreg,
         incidence_sexpaid12m = infections_sexpaid12m/susceptible_sexpaid12m)

datolder <- alldat %>%
  filter(age_group %in% c("Y025_029","Y030_034","Y035_039","Y040_044",
                          "Y045_049")) %>%
  group_by(area_id,iso3,area_level) %>%
  summarise(age_group = "Y025_049",
            population = sum(population),
            plhiv = sum(plhiv),
            infections = sum(infections),
            population_nosex12m = sum(population_nosex12m),
            population_sexcohab = sum(population_sexcohab),
            population_sexnonreg = sum(population_sexnonreg),
            population_sexpaid12m = sum(population_sexpaid12m),
            plhiv_nosex12m = sum(plhiv_nosex12m),
            plhiv_sexcohab = sum(plhiv_sexcohab),
            plhiv_sexnonreg = sum(plhiv_sexnonreg),
            plhiv_sexpaid12m = sum(plhiv_sexpaid12m),
            susceptible_nosex12m = sum(susceptible_nosex12m),
            susceptible_sexcohab = sum(susceptible_sexcohab),
            susceptible_sexnonreg = sum(susceptible_sexnonreg),
            susceptible_sexpaid12m = sum(susceptible_sexpaid12m),
            infections_nosex12m = sum(infections_nosex12m),
            infections_sexcohab = sum(infections_sexcohab),
            infections_sexnonreg = sum(infections_sexnonreg),
            infections_sexpaid12m = sum(infections_sexpaid12m))

datolder <- datolder %>%
  mutate(nosex12m = susceptible_nosex12m/(population-plhiv),
         sexcohab = susceptible_sexcohab/(population-plhiv),
         sexnonregplus = sum(susceptible_sexnonreg,susceptible_sexpaid12m)/(population-plhiv),
         sexnonreg = susceptible_sexnonreg/(population-plhiv),
         sexpaid12m = susceptible_sexpaid12m/(population-plhiv),
         incidence = (infections/(population-plhiv))*100,
         incidence_cat = cut(
           incidence,
           c(0, 0.3, 1, 3, 10^6),
           labels = c("Low", "Moderate", "High", "Very High"),
           include.lowest = TRUE,
           right = TRUE
         ),
         prev_nosex12m = plhiv_nosex12m/(susceptible_nosex12m + plhiv_nosex12m),
         prev_sexcohab = plhiv_sexcohab/(susceptible_sexcohab + plhiv_sexcohab),
         prev_sexnonreg = plhiv_sexnonreg/(susceptible_sexnonreg + plhiv_sexnonreg),
         prev_sexpaid12m = plhiv_sexpaid12m/(susceptible_sexpaid12m + plhiv_sexpaid12m),
         rr_sexpaid12m = NA,
         incidence_nosex12m = infections_nosex12m/susceptible_nosex12m,
         incidence_sexcohab = infections_sexcohab/susceptible_sexcohab,
         incidence_sexnonreg = infections_sexnonreg/susceptible_sexnonreg,
         incidence_sexpaid12m = infections_sexpaid12m/susceptible_sexpaid12m)

dat1549 <- alldat %>%
  filter(age_group %in% c("Y025_029","Y030_034","Y035_039","Y040_044",
                          "Y045_049", "Y015_019", "Y020_024")) %>%
  group_by(area_id,iso3,area_level) %>%
  summarise(age_group = "Y015_049",
            population = sum(population),
            plhiv = sum(plhiv),
            infections = sum(infections),
            population_nosex12m = sum(population_nosex12m),
            population_sexcohab = sum(population_sexcohab),
            population_sexnonreg = sum(population_sexnonreg),
            population_sexpaid12m = sum(population_sexpaid12m),
            plhiv_nosex12m = sum(plhiv_nosex12m),
            plhiv_sexcohab = sum(plhiv_sexcohab),
            plhiv_sexnonreg = sum(plhiv_sexnonreg),
            plhiv_sexpaid12m = sum(plhiv_sexpaid12m),
            susceptible_nosex12m = sum(susceptible_nosex12m),
            susceptible_sexcohab = sum(susceptible_sexcohab),
            susceptible_sexnonreg = sum(susceptible_sexnonreg),
            susceptible_sexpaid12m = sum(susceptible_sexpaid12m),
            infections_nosex12m = sum(infections_nosex12m),
            infections_sexcohab = sum(infections_sexcohab),
            infections_sexnonreg = sum(infections_sexnonreg),
            infections_sexpaid12m = sum(infections_sexpaid12m))

dat1549 <- dat1549 %>%
  mutate(nosex12m = susceptible_nosex12m/(population-plhiv),
         sexcohab = susceptible_sexcohab/(population-plhiv),
         sexnonregplus = sum(susceptible_sexnonreg,susceptible_sexpaid12m)/(population-plhiv),
         sexnonreg = susceptible_sexnonreg/(population-plhiv),
         sexpaid12m = susceptible_sexpaid12m/(population-plhiv),
         incidence = (infections/(population-plhiv))*100,
         incidence_cat = cut(
           incidence,
           c(0, 0.3, 1, 3, 10^6),
           labels = c("Low", "Moderate", "High", "Very High"),
           include.lowest = TRUE,
           right = TRUE
         ),
         prev_nosex12m = plhiv_nosex12m/(susceptible_nosex12m + plhiv_nosex12m),
         prev_sexcohab = plhiv_sexcohab/(susceptible_sexcohab + plhiv_sexcohab),
         prev_sexnonreg = plhiv_sexnonreg/(susceptible_sexnonreg + plhiv_sexnonreg),
         prev_sexpaid12m = plhiv_sexpaid12m/(susceptible_sexpaid12m + plhiv_sexpaid12m),
         rr_sexpaid12m = NA,
         incidence_nosex12m = infections_nosex12m/susceptible_nosex12m,
         incidence_sexcohab = infections_sexcohab/susceptible_sexcohab,
         incidence_sexnonreg = infections_sexnonreg/susceptible_sexnonreg,
         incidence_sexpaid12m = infections_sexpaid12m/susceptible_sexpaid12m)


outdat <- bind_rows(alldat,dat, datolder, dat1549)

outdat$concat <- paste0(outdat$area_id,outdat$age_group)

outdat <- outdat %>% select(-rr_sexnonreg)

# REMOVE MOZAMBIQUE HERE SINCE SI DOESN'T WANT THESE CIRCULATED
# outdat <- outdat %>% filter(iso3!="MOZ")

outdat <- outdat %>%
  select(area_id, age_group, concat, everything())

write_csv(outdat,"incidence-district-sexbehav_allages.csv")

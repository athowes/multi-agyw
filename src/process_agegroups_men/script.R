#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("process_agegroups_men")
# setwd("src/process_agegroups_men")

alldat <- read_csv("depends/incidence-district-sexbehav.csv")

alldat <- alldat %>% select(-msm_pr,-pwid_pr)

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
            population_msm = sum(population_msm),
            population_pwid = sum(population_pwid),
            plhiv_nosex12m = sum(plhiv_nosex12m),
            plhiv_sexcohab = sum(plhiv_sexcohab),
            plhiv_sexnonreg = sum(plhiv_sexnonreg),
            plhiv_msm = sum(plhiv_msm),
            plhiv_pwid = sum(plhiv_pwid),
            susceptible_nosex12m = sum(susceptible_nosex12m),
            susceptible_sexcohab = sum(susceptible_sexcohab),
            susceptible_sexnonreg = sum(susceptible_sexnonreg),
            susceptible_msm = sum(susceptible_msm),
            susceptible_pwid = sum(susceptible_pwid),
            infections_nosex12m = sum(infections_nosex12m),
            infections_sexcohab = sum(infections_sexcohab),
            infections_sexnonreg = sum(infections_sexnonreg),
            infections_msm = sum(infections_msm),
            infections_pwid = sum(infections_pwid))

dat <- dat %>%
  mutate(nosex12m = susceptible_nosex12m/(population-plhiv),
         sexcohab = susceptible_sexcohab/(population-plhiv),
         sexnonregplus = sum(susceptible_sexnonreg)/(population-plhiv),
         sexnonreg = susceptible_sexnonreg/(population-plhiv),
         msm = susceptible_msm/(population - plhiv),
         pwid = susceptible_pwid/(population - plhiv),
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
         prev_msm = plhiv_msm/(susceptible_msm + plhiv_msm),
         prev_pwid = plhiv_pwid/(susceptible_pwid + plhiv_pwid),
         rr_msm = NA,
         rr_pwid = NA,
         incidence_nosex12m = infections_nosex12m/susceptible_nosex12m,
         incidence_sexcohab = infections_sexcohab/susceptible_sexcohab,
         incidence_sexnonreg = infections_sexnonreg/susceptible_sexnonreg,
         incidence_msm = infections_msm/susceptible_msm,
         incidence_pwid = infections_pwid/susceptible_pwid)

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
            population_msm = sum(population_msm),
            population_pwid = sum(population_pwid),
            plhiv_nosex12m = sum(plhiv_nosex12m),
            plhiv_sexcohab = sum(plhiv_sexcohab),
            plhiv_sexnonreg = sum(plhiv_sexnonreg),
            plhiv_msm = sum(plhiv_msm),
            plhiv_pwid = sum(plhiv_pwid),
            susceptible_nosex12m = sum(susceptible_nosex12m),
            susceptible_sexcohab = sum(susceptible_sexcohab),
            susceptible_sexnonreg = sum(susceptible_sexnonreg),
            susceptible_msm = sum(susceptible_msm),
            susceptible_pwid = sum(susceptible_pwid),
            infections_nosex12m = sum(infections_nosex12m),
            infections_sexcohab = sum(infections_sexcohab),
            infections_sexnonreg = sum(infections_sexnonreg),
            infections_msm = sum(infections_msm),
            infections_pwid = sum(infections_pwid))

datolder <- datolder  %>%
  mutate(nosex12m = susceptible_nosex12m/(population-plhiv),
         sexcohab = susceptible_sexcohab/(population-plhiv),
         sexnonregplus = sum(susceptible_sexnonreg)/(population-plhiv),
         sexnonreg = susceptible_sexnonreg/(population-plhiv),
         msm = susceptible_msm/(population - plhiv),
         pwid = susceptible_pwid/(population - plhiv),
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
         prev_msm = plhiv_msm/(susceptible_msm + plhiv_msm),
         prev_pwid = plhiv_pwid/(susceptible_pwid + plhiv_pwid),
         rr_msm = NA,
         rr_pwid = NA,
         incidence_nosex12m = infections_nosex12m/susceptible_nosex12m,
         incidence_sexcohab = infections_sexcohab/susceptible_sexcohab,
         incidence_sexnonreg = infections_sexnonreg/susceptible_sexnonreg,
         incidence_msm = infections_msm/susceptible_msm,
         incidence_pwid = infections_pwid/susceptible_pwid)

dat1549 <- alldat %>%
  filter(age_group %in% c("Y025_029","Y030_034","Y035_039","Y040_044",
                          "Y045_049","Y015_019","Y020_024")) %>%
  group_by(area_id,iso3,area_level) %>%
  summarise(age_group = "Y015_049",
            population = sum(population),
            plhiv = sum(plhiv),
            infections = sum(infections),
            population_nosex12m = sum(population_nosex12m),
            population_sexcohab = sum(population_sexcohab),
            population_sexnonreg = sum(population_sexnonreg),
            population_msm = sum(population_msm),
            population_pwid = sum(population_pwid),
            plhiv_nosex12m = sum(plhiv_nosex12m),
            plhiv_sexcohab = sum(plhiv_sexcohab),
            plhiv_sexnonreg = sum(plhiv_sexnonreg),
            plhiv_msm = sum(plhiv_msm),
            plhiv_pwid = sum(plhiv_pwid),
            susceptible_nosex12m = sum(susceptible_nosex12m),
            susceptible_sexcohab = sum(susceptible_sexcohab),
            susceptible_sexnonreg = sum(susceptible_sexnonreg),
            susceptible_msm = sum(susceptible_msm),
            susceptible_pwid = sum(susceptible_pwid),
            infections_nosex12m = sum(infections_nosex12m),
            infections_sexcohab = sum(infections_sexcohab),
            infections_sexnonreg = sum(infections_sexnonreg),
            infections_msm = sum(infections_msm),
            infections_pwid = sum(infections_pwid))

dat1549 <- dat1549  %>%
  mutate(nosex12m = susceptible_nosex12m/(population-plhiv),
         sexcohab = susceptible_sexcohab/(population-plhiv),
         sexnonregplus = sum(susceptible_sexnonreg)/(population-plhiv),
         sexnonreg = susceptible_sexnonreg/(population-plhiv),
         msm = susceptible_msm/(population - plhiv),
         pwid = susceptible_pwid/(population - plhiv),
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
         prev_msm = plhiv_msm/(susceptible_msm + plhiv_msm),
         prev_pwid = plhiv_pwid/(susceptible_pwid + plhiv_pwid),
         rr_msm = NA,
         rr_pwid = NA,
         incidence_nosex12m = infections_nosex12m/susceptible_nosex12m,
         incidence_sexcohab = infections_sexcohab/susceptible_sexcohab,
         incidence_sexnonreg = infections_sexnonreg/susceptible_sexnonreg,
         incidence_msm = infections_msm/susceptible_msm,
         incidence_pwid = infections_pwid/susceptible_pwid)


outdat <- bind_rows(alldat,dat, datolder, dat1549)

outdat$concat <- paste0(outdat$area_id,outdat$age_group)

# REMOVE MOZAMBIQUE HERE SINCE SI DOESN'T WANT THESE CIRCULATED
# outdat <- outdat %>% filter(iso3!="MOZ")

outdat <- outdat %>%
  select(area_id, age_group, concat, everything())

write_csv(outdat,"incidence-district-sexbehav_allages.csv")

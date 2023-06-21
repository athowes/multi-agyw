#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("process_age-disagg-pwid")
# setwd("src/process_age-disagg-pwid/")

priority_iso3 <- multi.utils::priority_iso3()

pse_clean <- readRDS("kplhiv_art.rds")
wpp_pop_clean <- readRDS("wpp2019_denom_m.rds")
naomi_pop <- readRDS("depends/naomi_pop.rds")

pse <- pse_clean$PWID$area %>% filter(indicator=="pse_count",
                               iso3 %in% priority_iso3)

wpp_pop <- wpp_pop_clean %>%
  filter(age_group %in% c("Y015_019","Y020_024","Y025_029","Y030_034",
                          "Y035_039","Y040_044","Y045_049")) %>%
  group_by(area_id,year) %>%
  summarize(population = sum(population)) %>%
  filter(year==2019)

# Right now the WPP2019 estimates have incorrect area specification for TZA and
# ETH and COD and CAF.  Will use Naomi pop as denominator for right now - TO BE FIXED
pse <- pse %>%
  left_join(wpp_pop %>% select(area_id,population)) %>%
  left_join(naomi_pop %>%
              filter(age_group=="15-49") %>%
              rename(population_naomi = population) %>%
              select(area_id,population_naomi))

pse <- pse %>%
  mutate(prop_pwid = ifelse(iso3 %in% c("TZA","ETH","COD","CAF"),
                            median / population_naomi,
                           median / population) ) %>%
  select(-population,-population_naomi,-indicator,-lower,-upper,-median)

pse$iso3 <- as.character(pse$iso3)

# No data for Haiti in Oli's estimates
# bringing in PSE from Key Pop Atlas - no data from Haiti, using PSE from
# Dominican Republic (LB = 0.01%, UB = 0.02%) use 0.015% (this is surely
# low, but don't have better data to use really...)
hti_pse <- 0.00015
# fix to fill in cabo delgado so it doesn't break all of our future code, give it
# mozambique mean proportion across all areas
moz_1_10_pse <- mean(pse$prop_pwid[pse$iso3=="MOZ"])
pse <- rbind(pse,c("HTI","HTI",hti_pse),c("MOZ","MOZ_1_10",moz_1_10_pse))
pse$prop_pwid <- as.numeric(pse$prop_pwid)

age_groups <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49")

pwid <- pse %>%
  mutate(age_group = "15-49") %>%
  left_join(select(naomi_pop, iso3, area_id, age_group, population)) %>%
  mutate(total_pwid = population * prop_pwid) %>%
  select(iso3, area_id, total_pwid, age_group)

# get rid of female PWID here
# this is sloppy - assuming a 1:1 distribution of women:men in 15-49 year olds
pwid$total_pwid <- pwid$total_pwid * 0.91

#' FSW age distribution parameters in ZAF from Thembisa
#' Downloaded from: https://www.thembisa.org/content/downloadPage/Thembisa4_3
gamma_mean <- 29.4
gamma_sd <- 7

#' gamma_mean = alpha / beta
#' gamma_variance = alpha / beta^2
beta <- gamma_mean / gamma_sd^2 #' rate
alpha <- gamma_mean * beta #' shape

# pdf("thembisa-fsw-age-dist.pdf", h = 5, w = 6.25)
#
# data.frame(x = 15:49, y = dgamma(15:49, shape = alpha, rate = beta)) %>%
#   ggplot(aes(x = x, y = y)) +
#   geom_line() +
#   labs(x = "Age", y = "", title = "Age distribution of FSW from Thembisa 4.3 (ZAF)") +
#   theme_minimal()
#
# dev.off()

#' Distribution function of the gamma
pwid_gamma <- data.frame(
  dist = diff(pgamma(c(15, 20, 25, 30, 35, 40, 45, 50), shape = alpha, rate = beta)),
  age_group = age_groups
) %>%
  mutate(dist = dist / sum(dist))

# get admin1 population

df <- data.frame()

for(x in unique(pwid$area_id)) {
  print(x)

  df_x <- data.frame(
    area_id = x,
    age_group = age_groups
  )

  df_x <- df_x %>%
    group_by(area_id, age_group) %>%
    left_join(
      naomi_pop,
      by = c("area_id", "age_group")
    ) %>%
    ungroup() %>%
    mutate(
      pop = population,
      pop_prop = pop / sum(pop)
    )

  df <- bind_rows(df, df_x)
}

df <- df %>%
  left_join(
    pwid_gamma,
    by = "age_group"
  ) %>%
  full_join(
    select(pwid,total_pwid, iso3, area_id),
    by = c("area_id", "iso3")
  ) %>%
  mutate(pwid = dist * total_pwid,
         pwid_prop = pwid / population) %>%
  select(-pop, -pop_prop, -dist, -total_pwid)

pdf("age-disagg-pwid.pdf", h = 5, w = 6.25)

df %>%
  filter(age_group != "15-49") %>%
  ggplot(aes(x = forcats::fct_rev(area_id), y = pwid_prop)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  facet_grid(~age_group) +
  scale_y_continuous(breaks = seq(0, 0.05, by = 0.025)) +
  coord_flip() +
  labs(x = "PWID proportion", y = "ISO3") +
  theme_minimal()

dev.off()

extended_cbpalette <- colorRampPalette(multi.utils::cbpalette())

pdf("age-disagg-pwid-line.pdf", h = 4, w = 6.25)

df %>%
  filter(age_group != "Y015_049") %>%
  ggplot(aes(x = age_group, y = pwid_prop, group = iso3, col = iso3)) +
  geom_line() +
  scale_color_manual(values = extended_cbpalette(n = n_distinct(df$iso3))) +
  labs(x = "Age group", y = "PWID proportion", col = "ISO3") +
  theme_minimal()

dev.off()

pdf("palette-extension.pdf", h = 4, w = 4)

scales::show_col(multi.utils::cbpalette())

scales::show_col(extended_cbpalette(n = n_distinct(df$iso3)))

dev.off()

write_csv(df, "pwid-estimates.csv")

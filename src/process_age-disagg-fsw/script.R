#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("process_age-disagg-fsw")
# setwd("src/process_age-disagg-fsw/")

priority_iso3 <- multi.utils::priority_iso3()

#' fsw_ntl_pse.csv are the old estimates from Oli
#' fsw_ntl.csv are the newer estimates from Oli
pse <- readRDS("kplhiv_art.rds")

pse <- pse$FSW$area %>% filter(indicator=="pse_prop",
                               iso3 %in% priority_iso3) %>%
  rename(prop_fsw = median) %>%
  select(-indicator,-lower,-upper)

afs <- readRDS("kinh-afs-dist.rds")
naomi_pop <- readRDS("depends/naomi_pop.rds")

pse$iso3 <- as.character(pse$iso3)

# No data for Haiti in Oli's estimates
# bringing in PSE from Key Pop Atlas - 2014 estimate is PSE of 176400 while
# 2015 estimate is PSE of 70300 - 176400 is super high, going with 70300 for now with
# 2015 wpp population denominator
# fix this so that it is not hard coded!!
# hti_pse <- 70300 / 2785000
# # fix to fill in cabo delgado so it doesn't break all of our future code, give it
# # mozambique mean proportion across all areas
# moz_1_10_pse <- mean(pse$prop_fsw[pse$iso3=="MOZ"])
# pse <- rbind(pse,c("HTI","HTI",hti_pse),c("MOZ","MOZ_1_10",moz_1_10_pse))
# pse$prop_fsw <- as.numeric(pse$prop_fsw)

age_groups <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49")

fsw <- pse %>%
  mutate(age_group = "15-49") %>%
  left_join(select(naomi_pop, iso3, area_id, age_group, population)) %>%
  mutate(total_fsw = population * prop_fsw) %>%
  select(iso3, area_id, total_fsw, age_group)

#' FSW age distribution parameters in ZAF from Thembisa
#' Downloaded from: https://www.thembisa.org/content/downloadPage/Thembisa4_3
gamma_mean <- 29
gamma_sd <- 9

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
zaf_gamma <- data.frame(
  dist = diff(pgamma(c(15, 20, 25, 30, 35, 40, 45, 50), shape = alpha, rate = beta)),
  age_group = age_groups
) %>%
  mutate(dist = dist / sum(dist))

#' Just take the yob to be 2000 for now, could be improved later
cohort <- 2000

afs <- afs %>%
  rename(area_id = ISO_A3) %>%
  filter(
    yob == cohort,
    sex == "female",
    area_id %in% priority_iso3
  )

# No data for Haiti in Kinh's estiamtes - borrow Zambia's estimate (looks
# similar on statcompiler) - get correct estimate later!
# afs <- rbind(afs,afs[afs$area_id=="ZMB",])
# afs$area_id[nrow(afs)] <- "HTI"

pskewlogis <- function(t, scale, shape, skew) {
  (1 + (scale * t)^-shape)^-skew
}

# need to assign kinh's afs to all admin1s in a country
# then merge to admin1 population

afs <- afs %>%
  rename(iso3 = area_id)

afs <- afs %>%
  full_join(select(fsw,iso3,area_id))

df <- data.frame()

for(x in unique(afs$area_id)) {
  print(x)
  afs_x <- filter(afs, area_id == x)
  ages <- 15:49

  df_x <- data.frame(
    area_id = x,
    age = ages,
    eversex = pskewlogis(
      ages,
      scale = afs_x$lambda,
      skew = afs_x$skew,
      shape = afs_x$shape
    ),
    age_group = rep(age_groups, each = 5)
  )

  df_x <- df_x %>%
    group_by(area_id, age_group) %>%
    summarise(eversex = mean(eversex)) %>%
    left_join(
      naomi_pop,
      by = c("area_id", "age_group")
    ) %>%
    mutate(
      eversexpop = eversex * population,
      eversexpop_prop = eversexpop / sum(eversexpop)
    )

  df <- bind_rows(df, df_x)
}

# for ZAF as a whole for reference to Thembisa
afs_zaf <- filter(afs, iso3 == "ZAF")
afs_zaf <- filter(afs_zaf, row_number()==1)
ages <- 15:49

df_zaf <- data.frame(
  iso3 = "ZAF",
  age = ages,
  eversex = pskewlogis(
    ages,
    scale = afs_zaf$lambda,
    skew = afs_zaf$skew,
    shape = afs_zaf$shape
  ),
  age_group = rep(age_groups, each = 5)
)

df_zaf <- df_zaf %>%
  group_by(iso3, age_group) %>%
  summarise(eversex = mean(eversex)) %>%
  left_join(
    naomi_pop,
    by = c("iso3" = "area_id", "age_group")
  ) %>%
  mutate(
    eversexpop = eversex * population,
    eversexpop_prop = eversexpop / sum(eversexpop)
  )

#' Calculate propensities based on ZAF
propensity <- df_zaf %>%
  filter(iso3 == "ZAF") %>%
  left_join(
    zaf_gamma,
    by = "age_group"
  ) %>%
  mutate(propensity = dist / eversexpop_prop) %>%
  ungroup() %>%
  select(age_group, propensity)

#' Compute proportions for each age group for other countries
df <- df %>%
  left_join(
    propensity,
    by = "age_group"
  ) %>%
  mutate(dist = eversexpop_prop * propensity) %>%
  group_by(area_id) %>%
  mutate(dist = dist / sum(dist)) %>%
  ungroup()

df <- select(fsw, total_fsw, iso3, area_id) %>%
  full_join(
    df,
    by = c("area_id", "iso3")
  ) %>%
  mutate(
    fsw = dist * total_fsw,
    fsw_prop = fsw / population
  ) %>%
  select(-eversexpop, -eversexpop_prop, -propensity, - dist, -total_fsw)

# pdf("age-disagg-fsw.pdf", h = 5, w = 6.25)
#
# df %>%
#   filter(age_group != "Y015_049") %>%
#   ggplot(aes(x = forcats::fct_rev(area_id), y = fsw_prop)) +
#   geom_bar(stat = "identity", alpha = 0.7) +
#   facet_grid(~age_group) +
#   scale_y_continuous(breaks = seq(0, 0.05, by = 0.025)) +
#   coord_flip() +
#   labs(x = "FSW proportion", y = "ISO3") +
#   theme_minimal()
#
# dev.off()

# extended_cbpalette <- colorRampPalette(multi.utils::cbpalette())

# pdf("age-disagg-fsw-line.pdf", h = 4, w = 6.25)
#
# df %>%
#   filter(age_group != "Y015_049") %>%
#   ggplot(aes(x = age_group, y = fsw_prop, group = iso3, col = iso3)) +
#   geom_line() +
#   scale_color_manual(values = extended_cbpalette(n = n_distinct(df$iso3))) +
#   labs(x = "Age group", y = "FSW proportion", col = "ISO3") +
#   theme_minimal()
#
# dev.off()

# pdf("palette-extension.pdf", h = 4, w = 4)
#
# scales::show_col(multi.utils::cbpalette())
#
# scales::show_col(extended_cbpalette(n = 30))
#
# dev.off()

write_csv(df, "fsw-estimates.csv")

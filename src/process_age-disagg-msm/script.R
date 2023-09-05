#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("process_age-disagg-msm")
# setwd("src/process_age-disagg-msm/")

priority_iso3 <- multi.utils::priority_iso3()

pse_clean <- readRDS("kplhiv_art.rds")
naomi_pop <- readRDS("depends/naomi_pop.rds")

pse <- pse_clean$MSM$area %>% filter(indicator=="pse_prop",
                                      iso3 %in% priority_iso3) %>%
  rename(prop_msm = median) %>%
  select(-indicator,-lower,-upper)

pse$iso3 <- as.character(pse$iso3)

# No data for Haiti in Oli's estimates
# bringing in PSE from Key Pop Atlas - 2014 estimate is 68.4k, 2015 estimate is
# 30.9k - take average of these, use average of 2014 and 2015 WPP population for
# men aged 15-49 for Haiti as denominator
hti_pse <- ((68400+30900)/2) / ((2731000+2780000)/2)
# fix to fill in cabo delgado so it doesn't break all of our future code, give it
# mozambique mean proportion across all areas
moz_1_10_pse <- mean(pse$prop_msm[pse$iso3=="MOZ"])
pse <- rbind(pse,c("HTI","HTI",hti_pse),c("MOZ","MOZ_1_10",moz_1_10_pse))
pse$prop_msm <- as.numeric(pse$prop_msm)

age_groups <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49")

msm <- pse %>%
  mutate(age_group = "15-49") %>%
  left_join(select(naomi_pop, iso3, area_id, age_group, population)) %>%
  mutate(total_msm = population * prop_msm) %>%
  select(iso3, area_id, total_msm, age_group)

#' MSM age distribution parameters in ZAF from Thembisa
#' Downloaded from: https://www.thembisa.org/content/downloadPage/Thembisa4_3
gamma_mean <- 28
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
msm_gamma <- data.frame(
  dist = diff(pgamma(c(15, 20, 25, 30, 35, 40, 45, 50), shape = alpha, rate = beta)),
  age_group = age_groups
) %>%
  mutate(dist = dist / sum(dist))

# get admin1 population

df <- data.frame()

for(x in unique(msm$area_id)) {
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
    msm_gamma,
    by = "age_group"
  ) %>%
  full_join(
    select(msm,total_msm, iso3, area_id),
    by = c("area_id", "iso3")
  ) %>%
  mutate(msm = dist * total_msm,
         msm_prop = msm / population) %>%
  select(-pop, -pop_prop, -dist, -total_msm)

pdf("age-disagg-msm.pdf", h = 5, w = 6.25)

df %>%
  filter(age_group != "Y015_049") %>%
  ggplot(aes(x = forcats::fct_rev(area_id), y = msm_prop)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  facet_grid(~age_group) +
  scale_y_continuous(breaks = seq(0, 0.05, by = 0.025)) +
  coord_flip() +
  labs(x = "MSM proportion", y = "ISO3") +
  theme_minimal()

dev.off()

extended_cbpalette <- colorRampPalette(multi.utils::cbpalette())

pdf("age-disagg-msm-line.pdf", h = 4, w = 6.25)

df %>%
  filter(age_group != "Y015_049") %>%
  ggplot(aes(x = age_group, y = msm_prop, group = iso3, col = iso3)) +
  geom_line() +
  scale_color_manual(values = extended_cbpalette(n = n_distinct(df$iso3))) +
  labs(x = "Age group", y = "MSM proportion", col = "ISO3") +
  theme_minimal()

dev.off()

pdf("palette-extension.pdf", h = 4, w = 4)

scales::show_col(multi.utils::cbpalette())

scales::show_col(extended_cbpalette(n = n_distinct(df$iso3)))

dev.off()

write_csv(df, "msm-estimates.csv")

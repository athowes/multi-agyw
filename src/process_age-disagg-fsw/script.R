#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("process_age-disagg-fsw")
# setwd("src/process_age-disagg-fsw/")

priority_iso3 <- multi.utils::priority_iso3()

pse <- read_csv("fsw_ntl_pse.csv")
afs <- readRDS("kinh-afs-dist.rds")
pop <- readRDS("depends/naomi_pop.rds")

age_groups <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49")

fsw <- pse %>%
  select(-indicator, -lower, -upper) %>%
  rename(
    area_id = iso3,
    total_fsw = median
  ) %>%
  filter(area_id %in% priority_iso3) %>%
  mutate(age_group = "15-49")

#' FSW age distribution parameters in ZAF from Thembisa
#' Downloaded from: https://www.thembisa.org/content/downloadPage/Thembisa4_3
gamma_mean <- 29
gamma_sd <- 9

#' gamma_mean = alpha / beta
#' gamma_variance = alpha / beta^2
beta <- gamma_mean / gamma_sd^2 #'rate
alpha <- gamma_mean * beta #' shape

pdf("thembisa-fsw-age-dist.pdf", h = 5, w = 6.25)

data.frame(x = 15:49, y = dgamma(15:49, shape = alpha, rate = beta)) %>%
  ggplot(aes(x = x, y = y)) +
  geom_line() +
  labs(x = "Age", y = "", title = "Age distribution of FSW from Thembisa 4.3 (ZAF)") +
  theme_minimal()

dev.off()

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

pskewlogis <- function(t, scale, shape, skew) {
  (1 + (scale * t)^-shape)^-skew
}

df <- data.frame()

for(x in priority_iso3) {

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
      pop,
      by = c("area_id", "age_group")
    ) %>%
    mutate(
      eversexpop = eversex * population,
      eversexpop_prop = eversexpop / sum(eversexpop)
    )

  df <- bind_rows(df, df_x)
}

#' Calculate propensities based on ZAF
propensity <- df %>%
  filter(area_id == "ZAF") %>%
  left_join(
    zaf_gamma,
    by = "age_group"
  ) %>%
  mutate(propensity = dist / eversexpop_prop) %>%
  select(age_group, propensity)

#' Compute proportions for each age group for other countries
df <- df %>%
  left_join(
    propensity,
    by = "age_group"
  ) %>%
  mutate(dist = eversexpop_prop * propensity) %>%
  group_by(iso3) %>%
  mutate(dist = dist / sum(dist)) %>%
  ungroup()

df <- df %>%
  left_join(
    select(fsw, total_fsw, area_id),
    by = c("area_id")
  ) %>%
  mutate(
    fsw = dist * total_fsw,
    fsw_prop = fsw / population
  ) %>%
  select(-eversexpop, -eversexpop_prop, -propensity, - dist, -total_fsw)

pdf("age-disagg-fsw.pdf", h = 5, w = 6.25)

df %>%
  filter(age_group != "Y015_049") %>%
  ggplot(aes(x = forcats::fct_rev(area_id), y = fsw_prop)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  facet_grid(~age_group) +
  scale_y_continuous(breaks = seq(0, 0.05, by = 0.025)) +
  coord_flip() +
  theme_minimal()

dev.off()

extended_cbpalette <- colorRampPalette(multi.utils::cbpalette())

pdf("age-disagg-fsw-line.pdf", h = 4, w = 6.25)

df %>%
  filter(age_group != "Y015_049") %>%
  ggplot(aes(x = age_group, y = fsw_prop, group = area_id, col = area_id)) +
  geom_line() +
  scale_color_manual(values = extended_cbpalette(n = 13)) +
  labs(x = "Age group", y = "FSW proportion", col = "ISO3") +
  theme_minimal()

dev.off()

pdf("palette-extension.pdf", h = 4, w = 4)

scales::show_col(multi.utils::cbpalette())

scales::show_col(extended_cbpalette(n = 13))

dev.off()

write_csv(df, "fsw-estimates.csv")

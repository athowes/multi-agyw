#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("process_age-disagg-fsw")
# setwd("src/process_age-disagg-fsw/")

priority_iso3 <- multi.utils::priority_iso3()

pse <- read_csv("pse_estimates.csv")
afs <- readRDS("kinh-afs-dist.rds")
pop <- read_csv("depends/interpolated_population.csv")

age_groups <- c("Y015_019", "Y020_024", "Y025_029", "Y030_034", "Y035_039", "Y040_044", "Y045_049")

pop <- pop %>%
  filter(
    age_group %in% age_groups,
    sex == "female",
    area_id %in% priority_iso3,
    year == 2018,
  )

fsw <- pse %>%
  select(-"...1", -lower, -upper, -region, -four_region) %>%
  rename(area_id = iso3) %>%
  mutate(year = 2018) %>%
  filter(
    kp == "FSW",
    area_id %in% priority_iso3
  ) %>%
  left_join(
    pop %>%
      group_by(area_id, year) %>%
      summarise(population = sum(population)) %>%
      mutate(age_group = "Y015_049", .before = population),
    by = c("area_id", "year")
  ) %>%
  mutate(
    total_fsw = median * population
  )

#' Just take the yob to be 2000 for now, could be improved later
afs <- afs %>%
  rename(area_id = ISO_A3) %>%
  filter(
    yob == 2000,
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
    mutate(eversexpop = eversex * population)

  total_eversexpop <- sum(df_x$eversexpop)
  total_fsw <- filter(fsw, area_id == x)$total_fsw

  df_x <- df_x %>%
    mutate(
      fsw = (eversexpop / total_eversexpop) * total_fsw,
      fsw_prop = fsw / population
    )

  df <- bind_rows(df, df_x)
}

pdf("age-disagg-fsw.pdf", h = 5, w = 6.25)

ggplot(df, aes(x = forcats::fct_rev(area_id), y = fsw_prop)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  facet_grid(~age_group) +
  scale_y_continuous(breaks = seq(0, 0.05, by = 0.025)) +
  coord_flip() +
  theme_minimal()

dev.off()

write_csv(df, "fsw-estimates.csv")

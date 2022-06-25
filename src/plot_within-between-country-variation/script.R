#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("plot_within-between-country-variation")
# setwd("src/plot_within-between-country-variation")

df_3p1 <- read_csv("depends/adjust-best-3p1-multi-sexbehav-sae.csv") %>%
  filter(indicator != "sexnonregplus")
pop <- readRDS("depends/naomi_pop.rds")

#' Add population and updating naming
df_3p1 <- df_3p1 %>%
  filter(
    age_group != "15-24",
    year == 2018
  ) %>%
  multi.utils::update_naming() %>%
  left_join(
    select(pop, area_id, age_group, population_mean = population),
    by = c("area_id", "age_group")
  )

temp_3p1 <- prepare_estimates(df_3p1)
df_3p1_subnational <- temp_3p1$df_subnational
df_3p1_national <- temp_3p1$df_national

write_csv(df_3p1_subnational, "df_3p1_subnational.csv")
write_csv(df_3p1_national, "df_3p1_national.csv")

pdf("3p1-within-between-country-variation.pdf", h = 7, w = 6.25)

plotA <- df_3p1_subnational %>%
  mutate(iso3 = reorder(iso3, iso3_sort_order)) %>%
  ggplot(aes(x = fct_rev(iso3), y = estimate_smoothed, col = region)) +
    geom_jitter(width = 0.1, alpha = 0.6, shape = 20) +
    geom_point(data = df_3p1_national, aes(x = fct_rev(iso3), y = estimate_smoothed),
               shape = 21, size = 2, fill = "white", col = "black", alpha = 0.9) +
    facet_grid(age_group ~  indicator, labeller = labeller(indicator = label_wrap_gen(20))) +
    scale_color_manual(values = multi.utils::cbpalette()) +
    scale_y_continuous(labels = function(x) paste0(100 * x, "%")) +
    coord_flip() +
    labs(x = "", y = "Proportion", col = "Regions of sub-Saharan Africa") +
    guides(colour = guide_legend(override.aes = list(alpha = 0.9, size = 5))) +
    theme_minimal() +
    theme(
      panel.spacing = unit(0.5, "lines"),
      legend.position = "bottom",
      strip.text = element_text(face = "bold"),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 9),
    )

plotA

dev.off()

#' Save version for poster
ggsave(
  "3p1-within-between-country-variation.png",
  plotA,
  width = 6.25, height = 7, units = "in", dpi = 300
)

#' Version showing cohabiting numbers for 15-19

pdf("cohabiting-girls.pdf", h = 4, w = 6.25)

df_3p1_subnational %>%
  mutate(iso3 = reorder(iso3, iso3_sort_order)) %>%
  filter(
    age_group == "15-19",
    indicator == "One cohabiting partner"
  ) %>%
  mutate(
    moz_indicator = factor(ifelse(iso3 == "Mozambique", 1, 0))
  ) %>%
  ggplot(aes(x = fct_rev(iso3), y = estimate_smoothed, col = moz_indicator)) +
  geom_jitter(width = 0.1, shape = 20) +
  geom_point(data = filter(df_3p1_national, age_group == "15-19", indicator == "Cohabiting partner"),
             aes(x = fct_rev(iso3), y = estimate_smoothed), shape = 21, size = 2, fill = "white",
             col = "black", alpha = 0.9) +
  scale_color_manual(values = c("#D3D3D3", multi.utils::cbpalette()[2])) +
  scale_y_continuous(labels = function(x) paste0(100 * x, "%")) +
  coord_flip() +
  labs(x = "", y = "Proportion of girls 15-19 cohabiting", col = "") +
  guides(colour = "none") +
  theme_minimal() +
  theme(
    panel.spacing = unit(0.5, "lines"),
    legend.position = "bottom",
    strip.text = element_text(face = "bold"),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 9),
  )

dev.off()

#' Have a closer look at the FSW estimates and check that the adjustment has worked
df_3p1_unadjusted <- read_csv("depends/best-3p1-multi-sexbehav-sae.csv")

#' Add population and updating naming
df_3p1_unadjusted <- df_3p1_unadjusted %>%
  filter(
    age_group != "15-24",
    year == 2018
  ) %>%
  multi.utils::update_naming() %>%
  left_join(
    select(pop, area_id, age_group, population_mean = population),
    by = c("area_id", "age_group")
  )

temp_3p1_unadjusted <- prepare_estimates(df_3p1_unadjusted)

df_subnational_fsw <- bind_rows(
  mutate(df_3p1_subnational, adjustment = "Adjusted"),
  mutate(temp_3p1_unadjusted$df_subnational, adjustment = "Not adjusted")
)

df_national_fsw <- bind_rows(
  mutate(df_3p1_national, adjustment = "Adjusted"),
  mutate(temp_3p1_unadjusted$df_national, adjustment = "Not adjusted")
)

pdf("fsw-within-between-country-variation.pdf", h = 6, w = 6.25)

plotB <- df_subnational_fsw %>%
  filter(indicator == "FSW") %>%
  mutate(iso3 = reorder(iso3, iso3_sort_order)) %>%
  ggplot(aes(x = fct_rev(iso3), y = estimate_smoothed, col = region)) +
  geom_jitter(width = 0.1, alpha = 0.6, shape = 20) +
  geom_point(data = filter(df_national_fsw, indicator == "FSW"), aes(x = fct_rev(iso3), y = estimate_smoothed),
             shape = 21, size = 2, fill = "white", col = "black", alpha = 0.9) +
  facet_grid(adjustment ~ age_group) +
  scale_color_manual(values = multi.utils::cbpalette()) +
  scale_y_continuous(labels = function(x) paste0(100 * x, "%")) +
  coord_flip() +
  labs(x = "", y = "Proportion", col = "Regions of sub-Saharan Africa") +
  guides(colour = guide_legend(override.aes = list(alpha = 0.9, size = 5))) +
  theme_minimal() +
  theme(
    panel.spacing = unit(1, "lines"),
    legend.position = "bottom",
    strip.text = element_text(face = "bold"),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 9),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

plotB

dev.off()

ggsave(
  "fsw-within-between-country-variation.png",
  plotB,
  width = 6.25, height = 6, units = "in", dpi = 300
)

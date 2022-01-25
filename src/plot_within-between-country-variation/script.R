#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("plot_within-between-country-variation")
# setwd("src/plot_within-between-country-variation")

df <- read_csv("depends/best-3-multinomial-smoothed-district-sexbehav.csv")

df <- df %>%
  filter(
    age_group != "Y015_024"
  ) %>%
  mutate(
    #' Assuming the survey_id is structured as ISO2000DHS
    year = substr(survey_id, 4, 7),
    #' Labels for plot
    age_group = fct_relevel(age_group, "Y015_024", after = 3) %>%
      fct_recode(
        "15-19" = "Y015_019",
        "20-24" = "Y020_024",
        "25-29" = "Y025_029",
        "15-24" = "Y015_024"
      ),
    indicator =
      fct_recode(indicator,
        "No sex (past 12 months)" = "nosex12m",
        "Cohabiting partner" = "sexcohab",
        "Nonregular partner(s)" = "sexnonregplus"
      ),
    iso3 =
      fct_recode(iso3,
        "Botswana" = "BWA",
        "Cameroon" = "CMR",
        "Kenya" = "KEN",
        "Lesotho" = "LSO",
        "Mozambique" = "MOZ",
        "Malawi" = "MWI",
        "Namibia" = "NAM",
        "Eswatini" = "SWZ",
        "Tanzania" = "TZA",
        "Uganda" = "UGA",
        "South Africa" = "ZAF",
        "Zambia" = "ZMB",
        "Zimbabwe" = "ZWE"
    )
  ) %>%
  #' Only the most recent survey in each year
  group_by(iso3) %>%
  filter(year == max(year)) %>%
  ungroup()

df_subnational <- df %>%
  filter(
    !(area_id %in% c(
      "BWA", "CMR", "KEN", "LSO", "MOZ", "MWI", "NAM",
      "SWZ", "TZA", "UGA", "ZAF", "ZMB", "ZWE"
      )
    )
  )

df_national <- setdiff(df, df_subnational)

#' Countries that are missing a national level aggregate
#' This is a temporary solution and we should go back over and get these aggregates in properly
missing_national <- df_national %>%
  filter(is.na(estimate_smoothed)) %>%
  pull(iso3) %>%
  unique()

#' Overwriting NAs left_join (there must be a better way to do this, looks a lot simpler in data.table)
df_national <- left_join(
  df_national,
  df_subnational %>%
    filter(iso3 %in% missing_national) %>%
    group_by(iso3, age_group, indicator) %>%
    summarise(estimate_smoothed = mean(estimate_smoothed)),
  by = c("iso3", "age_group", "indicator")
) %>%
  within(., estimate_smoothed.x <- ifelse(!is.na(estimate_smoothed.y), estimate_smoothed.y, estimate_smoothed.x)) %>%
  select(-estimate_smoothed.y) %>%
  rename(estimate_smoothed = estimate_smoothed.x)

#' Add region column
#' Defined based on the UN geoscheme for Africa
#' https://en.wikipedia.org/wiki/United_Nations_geoscheme_for_Africa
region_key <- c(
  "Botswana" = "South",
  "Cameroon" = "Middle",
  "Kenya" = "East",
  "Lesotho" = "South",
  "Mozambique" = "East",
  "Malawi" = "East",
  "Namibia" = "South",
  "Eswatini" = "South",
  "Tanzania" = "East",
  "Uganda" = "East",
  "South Africa" = "South",
  "Zambia" = "East",
  "Zimbabwe" = "East"
) %>%
  as.data.frame() %>%
  rename("Region" = ".") %>%
  tibble::rownames_to_column("iso3")

df_subnational <- df_subnational %>%
  left_join(region_key, by = "iso3")

df_national <- df_national %>%
  left_join(region_key, by = "iso3")

pdf("within-between-country-variation.pdf", h = 7, w = 6.25)

cbpalette <- c("#56B4E9","#009E73", "#E69F00", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")

ggplot(df_subnational, aes(x = fct_rev(iso3), y = estimate_smoothed, col = Region)) +
  geom_jitter(width = 0.1, alpha = 0.6, shape = 20) +
  geom_point(data = df_national, aes(x = fct_rev(iso3), y = estimate_smoothed),
             shape = 21, size = 2, fill = "white", col = "black", alpha = 0.9) +
  facet_grid(age_group ~  indicator) +
  scale_color_manual(values = cbpalette) +
  scale_y_continuous(labels = function(x) paste0(100 * x, "%")) +
  coord_flip() +
  labs(x = "", y = "Proportion", col = "UN geoscheme region") +
  guides(colour = guide_legend(override.aes = list(alpha = 0.9, size = 5))) +
  theme_minimal() +
  theme(
    panel.spacing = unit(1.5, "lines"),
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )

dev.off()

#' Quantification of points discussed
#' What proportion of 15-19 year-olds are sexually active? And other averages for use in the abstract
df_national %>%
  select(iso3, indicator, age_group, estimate_smoothed) %>%
  group_by(indicator, age_group) %>%
  summarise(
    lower = 100 * quantile(estimate_smoothed, probs = 0.025),
    mean = 100 * mean(estimate_smoothed),
    upper = 100 * quantile(estimate_smoothed, probs = 0.975)
  )

#' What proportion of 15-19 year-olds are cohabiting in MOZ?
df_national %>%
  filter(
    indicator == "Cohabiting partner",
    age_group == "15-19",
    iso3 == "Mozambique"
  ) %>%
  mutate(estimate_smoothed = 100 * estimate_smoothed) %>%
  pull(estimate_smoothed) %>%
  signif(digits = 3)

#' What proportion of 20-29 year-olds are sexually active?
df_national %>%
  filter(
    indicator == "No sex (past 12 months)",
    age_group %in% c("20-24", "25-29")
  ) %>%
  summarise(
    lower = 100 * quantile(estimate_smoothed, probs = 0.025),
    mean = 100 * mean(estimate_smoothed),
    upper = 100 * quantile(estimate_smoothed, probs = 0.975)
  ) %>%
  signif(digits = 3)

#' What proportion of 20-29 year-olds are cohabiting versus with nonregular partner(s),
#' above and below the south / east dividing border (UN geoscheme)?
df_national %>%
  filter(
    indicator %in% c("Cohabiting partner", "Nonregular partner(s)"),
    age_group %in% c("20-24", "25-29")
  ) %>%
  mutate(
    border = ifelse(Region %in% c("East", "Middle"), "Above", "Below")
  ) %>%
  group_by(border, indicator) %>%
  summarise(
    lower = 100 * quantile(estimate_smoothed, probs = 0.025),
    mean = 100 * mean(estimate_smoothed),
    upper = 100 * quantile(estimate_smoothed, probs = 0.975)
  ) %>%
  mutate(across(lower:upper, ~signif(.x, digits = 3)))

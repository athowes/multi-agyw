#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("plot_infections-reached")
# setwd("src/plot_infections-reached")

df <- read_csv("depends/incidence-district-sexbehav.csv")

#' Assume some value for how much a hypothetical intervention reduces incidence by (multiplicatively)
#' A value of one corresponds to "finding" an infection (or infections averted if the intervention is 100% effective)
p <- 1

df_area_age_behav <- df %>%
  select(iso3, area_id, age_group, starts_with("population_sex"), starts_with("incidence_sex")) %>%
  pivot_longer(
    cols = starts_with("population") | starts_with("incidence"),
    names_to = c("indicator", "category"),
    names_sep = "_"
  ) %>%
  pivot_wider(
    id_cols = c("iso3", "area_id", "age_group", "indicator", "category"),
    names_from = "indicator",
    values_from = "value"
  )

inf <- infections_reached_all_stratifications(df_area_age_behav)

#' Infections reached above baseline
pdf("infections-reached-above-baseline.pdf", h = 3.5, w = 6.25)

plot_infections_reached_above_baseline(inf)

dev.off()

ggsave(
  "infections-reached-above-baseline.png",
  plot_infections_reached_above_baseline(inf) + theme(legend.position = "bottom"),
  width = 6.25, height = 3.5, units = "in", dpi = 300
)

#' Infections reached
pdf("infections-reached.pdf", h = 3.5, w = 6.25)

plot_infections_reached(inf)

dev.off()

#' Save version for poster
ggsave(
  "infections-reached.png",
  plot_infections_reached(inf),
  width = 6.25, height = 3.5, units = "in", dpi = 300
)

#' Separate analysis for each country
fct_reorg <- function(fac, ...) {
  fct_recode(fct_relevel(fac, ...), ...)
}

inf_country <- df_area_age_behav %>%
  mutate(
    iso3 = fct_reorg(iso3,
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
  split(.$iso3) %>%
  lapply(function(x)
    infections_reached_all_stratifications(x) %>%
      mutate(iso3 = x$iso3[1])
  )

pdf("infections-reached-above-baseline-country.pdf", h = 3.5, w = 6.25)

lapply(inf_country, function(x)
  x %>%
    plot_infections_reached_above_baseline +
    labs(title = paste0(x$iso3[1]))
)

dev.off()

pdf("infections-reached-country.pdf", h = 3.5, w = 6.25)

plotsA <- lapply(inf_country, function(x)
  x %>%
    plot_infections_reached +
    labs(title = paste0(x$iso3[1]))
)

plotsA

dev.off()

#' Sadly multi-page .png don't exist
#' This is a bit clunky but unsure if there is a better option
lapply(1:length(plotsA), function(i) {
  ggsave(
    paste0("infections-reached-country-", i, ".png"),
    plotsA[[i]],
    width = 6.25, height = 3.5, units = "in", dpi = 300
  )
})

#' Quantification of points discussed

#' What is the minimum proportion of the population to reach in order to
#' achieve reaching over q \in [0, 1] proportion of the new infections?
min_pop_reached <- function(inf, q) {
  inf %>%
    filter(prop_infections_averted_cumulative > q) %>%
    group_by(stratification) %>%
    filter(prop_population_cumulative == min(prop_population_cumulative)) %>%
    select(stratification, prop_infections_averted_cumulative, prop_population_cumulative)
}

min_pop_reached(inf, 0.25)
min_pop_reached(inf, 0.5)

#' At a national level
country_min_pop <- lapply(inf_country, function(x) min_pop_reached(x, q = 0.25)) %>%
  bind_rows(.id = "country") %>%
  filter(stratification %in% c("Area, age, behaviour", "Area, age"))

country_min_pop %>%
  group_by(stratification) %>%
  summarise(
    max_prop_population_cumulative = max(prop_population_cumulative),
    median_prop_population_cumulative = median(prop_population_cumulative),
    min_prop_population_cumulative = min(prop_population_cumulative)
  )

#' What proportion of new infections are among FSW as compared with the proportion the population that are FSW?
inf %>%
  filter(
    stratification == "Behaviour",
    category == "sexpaid12m"
  ) %>%
  select(prop_infections_averted_cumulative, prop_population_cumulative)

#' What about by country?
lapply(inf_country, function(x)
  x %>%
    filter(stratification == "Behaviour", category == "sexpaid12m") %>%
    select(prop_infections_averted_cumulative, prop_population_cumulative)
) %>%
  bind_rows(.id = "country")

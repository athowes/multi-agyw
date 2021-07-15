#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("check_sum-to-100-sexbehav-sae")
# setwd("src/check_sum-to-100-sexbehav-sae/")

#' Trying to determine if the estimates sum to 100%
df_uni <- read_csv("depends/smoothed-district-sexbehav.csv") %>%
  select(indicator, age_group, area_name, area_idx, mean)

#' Add rows for nosex12m and neversex indicators
df_uni <- bind_rows(
  df_uni,
  #' Add nosex12m indicator (1 - sex12m)
  df_uni %>%
    filter(indicator == "sex12m") %>%
    mutate(mean = 1 - mean,
           indicator = "nosex12m"),
  #' Add neversex indicator (1 - eversex)
  df_uni %>%
    filter(indicator == "eversex") %>%
    mutate(mean = 1 - mean,
           indicator = "neversex")
) %>%
  mutate(type = "univariate",
         model = "Model 1")

df_mul <- read_csv("depends/multinomial-smoothed-district-sexbehav.csv") %>%
  select(indicator, age_group, area_name, area_idx, mean, model) %>%
  mutate(type = "multivariate")

df <- rbind(df_uni, df_mul) %>%
  mutate(
    age_group = fct_relevel(age_group, "Y015_024") %>%
      fct_recode("15-24" = "Y015_024", "15-19" = "Y015_019", "20-24" = "Y020_024", "25-29" = "Y025_029")
  )

stacked_bar <- function(df) {
  ggplot(df, aes(fill = indicator, y = mean, x = area_name)) +
    geom_bar(position = "stack", stat = "identity") +
    facet_wrap(~age_group) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    scale_y_continuous(labels = scales::percent) +
    labs(title = paste0("Estimates: ", df$type[1], ", ", tolower(df$model[1]))) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          legend.position = "bottom",
          legend.key.width = unit(2, "lines"))
}

pdf("check100.pdf", h = 11, w = 8.5)

#' Univariate smoothed using neversex
df %>%
  filter(type == "univariate",
         indicator %in%  c("neversex", "sexcohab", "sexnonreg", "sexpaid12m")) %>%
  stacked_bar()

#' Univariate smoothed using nosex12m
df %>%
  filter(type == "univariate",
         indicator %in%  c("nosex12m", "sexcohab", "sexnonreg", "sexpaid12m")) %>%
  stacked_bar()

#' Multivariate smoothed with Model 1
df %>%
  filter(type == "multivariate",
         model == "Model 1") %>%
  stacked_bar()

#' Multivariate smoothed with Model 2
df %>%
  filter(type == "multivariate",
         model == "Model 2") %>%
  stacked_bar()

#' Multivariate smoothed with Model 3
#' TODO: Could make this lapply rather than typing it out manually
df %>%
  filter(type == "multivariate",
         model == "Model 3") %>%
  stacked_bar()

dev.off()

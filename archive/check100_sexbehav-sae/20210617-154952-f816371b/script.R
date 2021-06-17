#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("check100_sexbehav-sae")
# setwd("src/check100_sexbehav-sae/")

df <- read_csv("depends/smoothed-district-sexbehav.csv")

df <- select(df, indicator, age_group, area_name, area_idx, estimate)

stacked_bar <- function(df) {
  ggplot(df, aes(fill = indicator, y = estimate, x = area_name)) +
    geom_bar(position = "stack", stat = "identity") +
    facet_wrap(~age_group) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
}

pdf("check100.pdf", h = 11, w = 8.5)

stacked_bar(df)

df <- bind_rows(
  df,
  #' Add nosex12m indicator (1 - sex12m)
  df %>%
    filter(indicator == "sex12m") %>%
    mutate(estimate = 1 - estimate,
           indicator = "nosex12m"),
  #' Add neversex indicator (1 - eversex)
  df %>%
    filter(indicator == "eversex") %>%
    mutate(estimate = 1 - estimate,
           indicator = "neversex")
)

df %>%
  filter(!(indicator %in%  c("sti12m", "eversex", "sex12m", "nosex12m"))) %>%
  stacked_bar()

df %>%
  filter(!(indicator %in%  c("sti12m", "eversex", "sex12m", "neversex"))) %>%
  stacked_bar()

dev.off()

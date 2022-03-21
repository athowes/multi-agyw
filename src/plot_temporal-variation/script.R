#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("plot_temporal-variation")
# setwd("src/plot_temporal-variation")

df_3 <- read_csv("depends/best-3-multi-sexbehav-sae.csv") %>%
  multi.utils::update_naming()

df_3p1 <- read_csv("depends/best-3p1-multi-sexbehav-sae.csv") %>%
  multi.utils::update_naming()

pdf("3-temporal-interpolation.pdf", h = 4, w = 6.25)

df_3 %>%
  split(.$iso3) %>%
  lapply(function(x)
    plot_temporal_interpolation(x) +
      labs(title = paste0(x$iso3[1]))
  )

dev.off()

pdf("3p1-temporal-interpolation.pdf", h = 4, w = 6.25)

df_3p1 %>%
  split(.$iso3) %>%
  lapply(function(x)
    plot_temporal_interpolation(x) +
      labs(title = paste0(x$iso3[1]))
  )

dev.off()

#' df <- df %>%
#'   filter(age_group != "15-24") %>%
#'   mutate(
#'     #' Assuming the survey_id is structured as ISO2000DHS
#'     year = substr(survey_id, 4, 7)
#'   )
#'
#' df_subnational <- df %>%
#'   filter(!(area_id %in% multi.utils::priority_iso3()))
#'
#' df_national <- setdiff(df, df_subnational)
#'
#' #' Add columns to df_national for the quantiles of the subnational estimates
#' df_national <- df_national %>%
#'   left_join(
#'     df_subnational %>%
#'       group_by(iso3, age_group, indicator) %>%
#'       summarise(
#'         subnational_q975 = quantile(estimate_smoothed, probs = 0.975),
#'         subnational_q750 = quantile(estimate_smoothed, probs = 0.750),
#'         subnational_q250 = quantile(estimate_smoothed, probs = 0.250),
#'         subnational_q025 = quantile(estimate_smoothed, probs = 0.025)
#'       ),
#'     by = c("iso3", "age_group", "indicator")
#'   )
#'
#' pdf("temporal-variation-alt.pdf", h = 8.25, w = 11.75)
#'
#' ggplot(df_national, aes(x = year, y = estimate_smoothed, fill = iso3, group = area_name)) +
#'   geom_line(aes(col = iso3), size = 1.5) +
#'   geom_ribbon(aes(ymin = subnational_q025, ymax = subnational_q975), alpha = 0.1) +
#'   geom_ribbon(aes(ymin = subnational_q250, ymax = subnational_q750), alpha = 0.3) +
#'   facet_grid(age_group ~  indicator) +
#'   labs(x = "Year of survey", y = "Posterior mean proportion", col = "Country", fill = "Country") +
#'   theme_minimal() +
#'   theme(
#'     plot.title = element_text(face = "bold"),
#'     legend.position = "bottom",
#'     legend.key.width = unit(4, "lines"),
#'     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
#'   )
#'
#' dev.off()

df <- read_csv("depends/multi-sexbehav-sae.csv")

pdf("temporal-interpolation.pdf", h = 6, w = 6.25)

df_ribbon <- df %>%
  mutate(iso3 = substr(area_id, 1, 3)) %>%
  group_by(indicator, iso3, year, age_group) %>%
  summarise(
    mean_smoothed = mean(estimate_smoothed),
    lower_smoothed = quantile(estimate_smoothed, probs = 0.05),
    upper_smoothed = quantile(estimate_smoothed, probs = 0.95)
  )

df_data <- df %>%
  filter(!is.na(estimate_raw)) %>%
  group_by(indicator, survey_id, age_group) %>%
  summarise(mean_raw = mean(estimate_raw)) %>%
  mutate(
    iso3 = substr(survey_id, 1, 3),
    year = as.numeric(substr(survey_id, 4, 7)),
    type = substr(survey_id, 8, 11)
  )

df_ribbon %>%
  split(.$indicator) %>%
  lapply(function(x)
    ggplot(x, aes(x = year, y = mean_smoothed)) +
      geom_ribbon(aes(ymin = lower_smoothed, ymax = upper_smoothed), alpha = 0.5) +
      geom_line() +
      geom_point(
        data = filter(df_data, indicator == x$indicator[1]),
        aes(x = year, y = mean_raw, col = type)
      ) +
      facet_grid(iso3 ~ age_group) +
      theme_minimal() +
      labs(title = paste0(x$indicator[1]), x = "Year", y = "Estimate")
  )

dev.off()

#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("plot_temporal-variation")
# setwd("src/plot_temporal-variation")

df_3 <- read_csv("depends/human-best-3-multinomial-smoothed-district-sexbehav.csv")
df_3p1 <- read_csv("depends/human-best-3p1-multinomial-smoothed-district-sexbehav.csv")

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

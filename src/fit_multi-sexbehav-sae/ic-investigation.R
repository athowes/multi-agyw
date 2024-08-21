cpo_fail <- lapply(res_fit, function(fit) fit$cpo$failure)
lapply(cpo_fail, table)

res_df %>%
  group_by(model) %>%
  summarise(
    dic = sum(local_dic, na.rm = TRUE),
    dic_se = stats::sd(local_dic, na.rm = TRUE) * sqrt(sum(!is.na(local_dic))),
    waic = sum(local_waic, na.rm = TRUE),
    waic_se = stats::sd(local_waic, na.rm = TRUE) * sqrt(sum(!is.na(local_waic))),
    cpo = sum(local_cpo, na.rm = TRUE),
    cpo_se = stats::sd(local_cpo, na.rm = TRUE) * sqrt(sum(!is.na(local_cpo)))
  )

df <- res_df |>
  filter(!is.na(survey_id)) |>
  select(model, survey_id, local_dic, local_waic, local_cpo) |>
  group_by(survey_id, model) |>
  summarise(
    dic = sum(local_dic, na.rm = TRUE),
    dic_se = stats::sd(local_dic, na.rm = TRUE) * sqrt(sum(!is.na(local_dic))),
    waic = sum(local_waic, na.rm = TRUE),
    waic_se = stats::sd(local_waic, na.rm = TRUE) * sqrt(sum(!is.na(local_waic))),
    cpo = sum(local_cpo, na.rm = TRUE),
    cpo_se = stats::sd(local_cpo, na.rm = TRUE) * sqrt(sum(!is.na(local_cpo)))
  )

ic_list <- df %>%
  rename("dic_mean" = "dic", "waic_mean" = "waic", "cpo_mean" = "cpo") %>%
  pivot_longer(
    cols = starts_with(c("dic", "waic", "cpo")),
    names_to = "name",
    values_to = "value"
  ) %>%
  separate(name, into = c("metric", "type"), extra = "merge", fill = "left") %>%
  pivot_wider(
    names_from = "type",
    values_from = "value"
  ) |>
  split(~ survey_id)

lapply(ic_list, function(x) {
  ggplot(x, aes(x = model, y = mean)) +
    geom_point(size = 3) +
    geom_errorbar(
      aes(ymin = mean - se, ymax = mean + se),
      stat = "identity", position = "dodge", alpha = 0.4, col = "black", width = 0
    ) +
    facet_wrap(~ metric, scales = "free") +
    theme_minimal()
})

four_better_three_cpo <- df |>
  split(~ survey_id) |>
  sapply(function(x) {
    x$cpo[4] > x$cpo[3]
  })

four_better_three_dic <- df |>
  split(~ survey_id) |>
  sapply(function(x) {
    x$dic[3] > x$dic[4]
  })

fbt_cpo <- names(four_better_three_cpo[four_better_three_cpo == TRUE])
fbt_dic <- names(four_better_three_dic[four_better_three_dic == TRUE])

setdiff(fbt_cpo, fbt_dic)
setdiff(fbt_dic, fbt_cpo)

tbf_cpo <- names(four_better_three_cpo[four_better_three_cpo == FALSE])
tbf_cpo

diff_dic <- df |>
  split(~ survey_id) |>
  sapply(function(x) {
    x$dic[3] - x$dic[4]
  })

plot(diff_dic)

which(diff_dic < -200)

diff_cpo <- df |>
  split(~ survey_id) |>
  sapply(function(x) {
    x$cpo[4] - x$cpo[3]
  })

plot(diff_cpo)

tza_dic <- as.data.frame(diff_dic) |>
  tibble::rownames_to_column("survey_id") |>
  mutate(
    iso3 = substr(survey_id, 1, 3),
    tza_survey = ifelse(iso3 == "TZA", TRUE, FALSE)
  ) |>
  ggplot(aes(x = survey_id, y = diff_dic, col = tza_survey)) +
    geom_point() +
    coord_flip() +
    labs(x = "", y = "M3 DIC - M4 DIC") +
    guides(col = "none") +
    theme_minimal()

ggsave("tza-dic.png", tza_dic, h = 6, w = 6.25)

tza_local_ic <- res_df |>
  filter(survey_id %in% c("TZA2007AIS", "TZA2010DHS", "TZA2012AIS")) |>
  select(model, survey_id, local_dic, local_waic, local_cpo)

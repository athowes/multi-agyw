#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("process_information-criteria")
# setwd("src/process_information-criteria")

#' aaa_fit_multi-sexbehav-sae

iso3 <- c("BWA", "CMR", "KEN", "LSO", "MOZ", "MWI", "NAM", "SWZ", "TZA", "UGA", "ZAF", "ZMB", "ZWE")
files <- paste0("depends/", tolower(iso3), "_information-criteria.csv")

df <- bind_rows(lapply(files, function(file) read_csv(file)))

write_csv(df, "model-comparison.csv", na = "")

df <- df %>%
  # #' Could clean the DIC for infeasible values here
  # mutate(dic = ifelse(abs(dic) > 10^5, NA, dic)) %>%
  #' Set names for plotting
  mutate(model =
           fct_recode(model,
             "1" = "Model 1: Constant",
             "2" = "Model 2: IID",
             "3" = "Model 3: BYM2"
            )
  ) %>%
  group_by(iso3) %>%
  #' Add best performing model indicator
  mutate(min_idx = (min(dic, na.rm = TRUE) == dic))

dic_plot <- function(df) {
  ggplot(df, aes(x = model,
                 y = dic,
                 ymin = dic - 1.96 * dic_se,
                 ymax = dic + 1.96 * dic_se,
                 col = min_idx)) +
    geom_pointrange(alpha = 0.7) +
    facet_wrap(~iso3, scales = "free") +
    scale_color_manual(values = c("black", "#E69F00")) +
    labs(x = "", y = "DIC",
         title = paste0("DIC results for the models in ", length(iso3), " countries"),
         subtitle = "Gold star indicates the best model(s) with the lowest DIC.\nMissing entries indicate that the DIC value returned was NA.",
         col = "Amongst best model(s)") +
    theme_minimal() +
    theme(
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold"),
      legend.position = "bottom",
      legend.key.width = unit(4, "lines")
    )
}

pdf("model-comparison.pdf", h = 5, w = 8.5)

dic_plot(df)

dev.off()

#' aaa_fit_all-dhs-multi-sexbehav-sae

iso3 <- c("CMR", "MOZ", "MWI", "ZMB", "ZWE")
files <- paste0("depends/", tolower(iso3), "_all-dhs-information-criteria.csv")

df <- bind_rows(lapply(files, function(file) read_csv(file)))

write_csv(df, "all-dhs-model-comparison.csv", na = "")

df <- df %>%
  #' Set names for plotting
  mutate(model =
           fct_recode(model,
                      "1" = "Model 1",
                      "2" = "Model 2",
                      "3" = "Model 3",
                      "4" = "Model 4",
                      "5" = "Model 5",
                      "6" = "Model 6",
                      "7" = "Model 7",
                      "8" = "Model 8",
                      "9" = "Model 9"
           )
  ) %>%
  group_by(iso3) %>%
  #' Add best performing model indicator
  mutate(min_idx = (min(dic, na.rm = TRUE) == dic))

pdf("all-dhs-model-comparison.pdf", h = 5, w = 8.5)

dic_plot(df)

dev.off()

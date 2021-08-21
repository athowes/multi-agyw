#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("process_information-criteria")
# setwd("src/process_information-criteria")

#' aaa_fit_multi-sexbehav-sae

iso3 <- c("BWA", "CMR", "KEN", "LSO", "MOZ", "MWI", "NAM", "SWZ", "TZA", "UGA", "ZAF", "ZMB", "ZWE")
files <- paste0("depends/", tolower(iso3), "_information-criteria.csv")

df <- bind_rows(lapply(files, function(file) read_csv(file)))

write_csv(df, "model-comparison.csv", na = "")

ic_plot <- function(df, ic = "dic") {

  df %>%
    # #' Could clean the DIC for infeasible values here
    # mutate(dic = ifelse(abs(dic) > 10^5, NA, dic)) %>%
    #' Set names for plotting
    mutate(model =
             fct_recode(model,
                        "1" = "Model 1: Constant",
                        "2" = "Model 2: IID",
                        "3" = "Model 3: BYM2",
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
    #' Add best performing model indicator (minimum and maximum)
    mutate(
      min_idx = (min(!!sym(ic), na.rm = TRUE) == !!sym(ic)),
      max_idx = (max(!!sym(ic), na.rm = TRUE) == !!sym(ic)),
      best_idx = if(ic %in% c("waic", "dic")) {min_idx} else {max_idx}
    ) %>%
    ggplot(aes(x = model,
               y = !!sym(ic),
               ymin = !!sym(ic) - 1.96 * !!sym(paste0(ic, "_se")),
               ymax = !!sym(ic) + 1.96 * !!sym(paste0(ic, "_se")),
               col = best_idx)) +
      geom_pointrange(alpha = 0.7) +
      facet_wrap(~iso3, scales = "free") +
      scale_color_manual(values = c("black", "#E69F00")) +
      labs(x = "", y = paste0(toupper(ic)),
           title = paste0(toupper(ic), " results for the models in ", length(iso3), " countries"),
           subtitle = "Missing entries indicate that the value returned was NA.",
           col = "Amongst best model(s)") +
      theme_minimal() +
      theme(
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        legend.position = "bottom",
        legend.key.width = unit(4, "lines")
      )
}

pdf("dic-model-comparison.pdf", h = 5, w = 8.5)

ic_plot(df)

dev.off()

#' aaa_fit_all-dhs-multi-sexbehav-sae

iso3 <- c("CMR", "MOZ", "MWI", "ZMB", "ZWE")
files <- paste0("depends/", tolower(iso3), "_all-dhs-information-criteria.csv")

df <- bind_rows(lapply(files, function(file) read_csv(file)))

write_csv(df, "all-dhs-model-comparison.csv", na = "")

pdf("all-dhs-dic-model-comparison.pdf", h = 5, w = 8.5)

ic_plot(df, ic = "dic")

dev.off()

pdf("all-dhs-cpo-model-comparison.pdf", h = 5, w = 8.5)

ic_plot(df, ic = "cpo")

dev.off()

pdf("all-dhs-pit-model-comparison.pdf", h = 5, w = 8.5)

ic_plot(df, ic = "pit")

dev.off()

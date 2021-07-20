#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("process_information-criteria")
# setwd("src/process_information-criteria")

iso3 <- c("CMR", "KEN", "LSO", "MOZ", "MWI", "NAM", "SWZ", "TZA", "UGA", "ZAF", "ZMB", "ZWE")
files <- paste0("depends/", tolower(iso3), "_information-criteria.csv")

df <- bind_rows(lapply(files, function(file) read_csv(file)))

write_csv(df, "all-model-comparison.csv", na = "")

df <- df %>%
  # #' Could cleaning the DIC for infeasible values here
  # mutate(dic = ifelse(abs(dic) > 10^5, NA, dic)) %>%
  #' Set names for plotting
  mutate(model =
           fct_recode(model,
             "Const" = "Model 1: Constant",
             "IID" = "Model 2: IID",
             "BYM2" = "Model 3: BYM2"
            )
  ) %>%
  group_by(iso3) %>%
  #' Add best performing model indicator
  mutate(min_idx = (min(dic, na.rm = TRUE) == dic))

pdf("all-model-comparison.pdf", h = 11, w = 8.5)

ggplot(df, aes(x = model, y = dic, col = min_idx, shape = min_idx)) +
  geom_point() +
  facet_wrap(~iso3, scales = "free") +
  #' Gold star to signify the best model
  scale_shape_manual(values = c(16, 8)) +
  scale_color_manual(values = c("black", "#E69F00")) +
  labs(x = "", y = "DIC",
       title = paste0("DIC results for the models in ", length(iso3), " countries"),
       subtitle = "Gold star indicates the best model(s) with the lowest DIC.\nMissing entries indicate that the DIC value returned was NA.") +
  theme_minimal() +
  theme(legend.position = "none")

dev.off()

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
    mutate(
      model = fct_recode(model,
        "1" = "Model 1: Constant", "2" = "Model 2: IID", "3" = "Model 3: BYM2",
        "1" = "Model 1", "2" = "Model 2", "3" = "Model 3",
        "4" = "Model 4", "5" = "Model 5", "6" = "Model 6",
        "7" = "Model 7", "8" = "Model 8", "9" = "Model 9"),
      iso3 = fct_recode(iso3,
        "Cameroon" = "CMR",
        "Kenya" = "KEN",
        "Lesotho" = "LSO",
        "Mozambique" = "MOZ",
        "Malawi" = "MWI",
        "Uganda" = "UGA",
        "Zambia" = "ZMB",
        "Zimbabwe" = "ZWE",
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
               col = best_idx,
               shape = best_idx)) +
      geom_pointrange(alpha = 0.7) +
      facet_wrap(~iso3, scales = "free") +
      scale_color_manual(values = c("black", "#E69F00")) +
      scale_shape_manual(values = c(16, 15)) +
      labs(x = "", y = paste0(toupper(ic)),
           title = paste0(toupper(ic), " results for the models in ", length(iso3), " countries"),
           subtitle = "Missing entries indicate that the value returned was NA.",
           col = "Best model(s)") +
      theme_minimal() +
      theme(
        strip.text = element_text(face = "bold"),
        legend.position = "bottom",
        legend.key.width = unit(4, "lines")
      )
}

pdf("dic-model-comparison.pdf", h = 5, w = 8.5)

ic_plot(df)

dev.off()

#' aaa_fit_all-dhs-multi-sexbehav-sae

iso3 <- c("CMR", "KEN", "LSO", "MOZ", "MWI", "UGA", "ZMB", "ZWE")
files <- paste0("depends/", tolower(iso3), "_all-dhs-information-criteria.csv")

df <- bind_rows(lapply(files, function(file) read_csv(file)))

write_csv(df, "all-dhs-model-comparison.csv", na = "")

#' DIC plot
pdf("all-dhs-dic-model-comparison.pdf", h = 5, w = 8.5)

ic_plot(df, ic = "dic")

dev.off()

#' WAIC plot
pdf("all-dhs-waic-model-comparison.pdf", h = 5, w = 8.5)

ic_plot(df, ic = "waic")

dev.off()

#' CPO plot
pdf("all-dhs-cpo-model-comparison.pdf", h = 5, w = 8.5)

ic_plot(df, ic = "cpo")

dev.off()

#' PIT plot
pdf("all-dhs-pit-model-comparison.pdf", h = 5, w = 8.5)

ic_plot(df, ic = "pit")

dev.off()

#' Create tables to output to LaTeX
df <- df %>%
  mutate(
    dic = paste0(dic, " (", dic_se, ")"),
    waic = paste0(waic, " (", waic_se, ")"),
    cpo = paste0(cpo, " (", dic_se, ")"),
    pit = paste0(pit, " (", pit_se, ")")
  ) %>%
  select(-contains("se")) %>%
  rename_with(~toupper(.), 3:6) %>%
  mutate(
    model = fct_recode(model,
      "1" = "Model 1", "2" = "Model 2", "3" = "Model 3",
      "4" = "Model 4", "5" = "Model 5", "6" = "Model 6",
      "7" = "Model 7", "8" = "Model 8", "9" = "Model 9"),
    iso3 = fct_recode(iso3,
      "Cameroon" = "CMR",
      "Kenya" = "KEN",
      "Lesotho" = "LSO",
      "Mozambique" = "MOZ",
      "Malawi" = "MWI",
      "Uganda" = "UGA",
      "Zambia" = "ZMB",
      "Zimbabwe" = "ZWE",
    )
  ) %>%
  rename(Model = model) %>%
  pivot_longer(
    cols = c("DIC", "WAIC", "CPO", "PIT"),
    names_to = "Criteria"
  ) %>%
  pivot_wider(
    names_from = "Model",
    values_from = "value"
  )

#' The column(s) which have the minimum value of the criteria
min_idx <- df %>%
  select(-iso3, -Criteria) %>%
  as.matrix() %>%
  #' Adding 2 because of the iso3 and Criteria columns
  apply(1, FUN = function(x) {
    cr <- sapply(x, stringr::word) %>%
      as.numeric()
    return(which(cr == min(cr)) + 2)
  })

#' The column(s) which have the maximum value of the criteria
max_idx <- df %>%
  select(-iso3, -Criteria) %>%
  as.matrix() %>%
  #' Adding 2 because of the iso3 and Criteria columns
  apply(1, FUN = function(x) {
    cr <- sapply(x, stringr::word) %>%
      as.numeric()
    return(which(cr == max(cr)) + 2)
  })


#' The column(s) which have the best value of the criteria
#' * DIC, WAIC this is min_idx
#' * CPO, PIT this is max_idx
best_idx <- min_idx

for(i in seq_along(df$Criteria)) {
  if(df$Criteria[[i]] %in% c("CPO", "PIT"))
  best_idx[[i]] <- max_idx[[i]]
}

tab <- gt(df, groupname_col = "iso3") %>%
  tab_spanner(
    label = "Model",
    columns = "1":"9"
  ) %>%
  #' It's clear from context that these are the criteria
  #' (such that the label is not required)
  cols_label(
    Criteria = "",
  ) %>%
  tab_stubhead(label = "")

#' Adding bold for best value to the table
for(i in seq_along(min_idx)) {
  tab <- tab %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(columns = best_idx[[i]], rows = i)
    )
}

#' Printing to a text file
#' At present this function is focused on the application of styles for HTML output only
#' (as such, other output formats will ignore all tab_style() calls) :(
tab %>%
  as_latex() %>%
  as.character() %>%
  cat(file = "all-dhs-model-comparison.txt")

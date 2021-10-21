ic_plot <- function(df, ic = "dic") {
  df %>%
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
         subtitle = "Missing entries indicate that the value returned was NA.") +
    theme_minimal() +
    theme(
      panel.spacing = unit(1.5, "lines"),
      legend.position = "none"
    )
}

rank_ic_plot <- function(df) {
  df %>%
    group_by(iso3) %>%
    mutate(
      dic_rank = rank(dic),
      waic_rank = rank(waic),
      #' Negative because we want desc = TRUE
      cpo_rank = rank(-cpo),
      pit_rank = rank(-pit)
    ) %>%
    ungroup() %>%
    group_by(model) %>%
    summarise(
      dic_rank_mean = mean(dic_rank),
      dic_rank_se = sd(dic_rank) / sqrt(n()),
      waic_rank_mean = mean(waic_rank),
      waic_rank_se = sd(waic_rank) / sqrt(n()),
      cpo_rank_mean = mean(cpo_rank),
      cpo_rank_se = sd(cpo_rank) / sqrt(n()),
      pit_rank_mean = mean(pit_rank),
      pit_rank_se = sd(pit_rank) / sqrt(n()),
    ) %>%
    ungroup() %>%
    pivot_longer(
      cols = -model,
      names_to = "metric",
      values_to = "value"
    ) %>%
    separate(metric, into = c("metric", "type"), extra = "merge", fill = "left") %>%
    pivot_wider(
      names_from = type,
      values_from = value
    ) %>%
    group_by(metric) %>%
    mutate(
      best_idx = (min(rank_mean, na.rm = TRUE) == rank_mean)
    ) %>%
    ungroup() %>%
    mutate(
      metric = fct_recode(metric,
                          "DIC" = "dic",
                          "WAIC" = "waic",
                          "CPO" = "cpo",
                          "PIT" = "pit"
      )
    ) %>%
    ggplot(aes(x = model, y = rank_mean, fill = metric, group = metric)) +
    facet_wrap(~metric) +
    geom_col(aes(alpha = best_idx), position = "dodge") +
    scale_alpha_discrete(range = c(0.6, 0.9)) +
    geom_errorbar(
      aes(ymin = rank_mean - rank_se, ymax = rank_mean + rank_se),
      stat = "identity", position = "dodge", alpha = 0.6, col = "black", width = 0.25
    ) +
    scale_fill_manual(values = cbpalette) +
    labs(x = "Model", y = "Average rank", fill = "Metric") +
    guides(color = FALSE, alpha = FALSE) +
    theme_minimal() +
    theme(
      panel.spacing = unit(1.5, "lines"),
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )
}

create_latex_table <- function(df, file_name) {
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
    cat(file = file_name)
}

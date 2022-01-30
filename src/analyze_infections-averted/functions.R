#' These two functions could probably be made into one with a by_iso3 TRUE or FALSE argument
compute_infections_averted <- function(df, stratification_name) {
  df %>%
    mutate(infections_averted = incidence * p * population) %>%
    #' Best opportunities have the highest incidence, take those first
    arrange(desc(incidence)) %>%
    mutate(
      population_cumulative = cumsum(population),
      infections_averted_cumulative = cumsum(infections_averted),
      stratification = stratification_name
    ) %>%
    bind_rows(
      data.frame(population_cumulative = 0, infections_averted_cumulative = 0, stratification = stratification_name)
    )
}

compute_infections_averted_by_country <- function(df, stratification_name) {
  df %>%
    mutate(infections_averted = incidence * p * population) %>%
    split(.$iso3) %>%
    lapply(function(x)
      x %>%
        #' Best opportunities have the highest incidence, take those first
        arrange(desc(incidence)) %>%
        mutate(
          population_cumulative = cumsum(population),
          infections_averted_cumulative = cumsum(infections_averted),
          stratification = stratification_name
        ) %>%
        bind_rows(
          data.frame(iso3 = x$iso3[1], population_cumulative = 0, infections_averted_cumulative = 0, stratification = stratification_name)
        )
    ) %>%
    bind_rows()
}

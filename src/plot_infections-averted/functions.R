compute_infections_averted <- function(df, stratification_name, by_country = FALSE) {

  #' Add infections averted column
  df <- mutate(df, infections_averted = incidence * p * population)

  if(by_country) {
  out <- df %>%
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
  } else {
  out <-   df %>%
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
  return(out)
}

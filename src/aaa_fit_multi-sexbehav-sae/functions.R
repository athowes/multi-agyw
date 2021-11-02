#' Compute the softmax of a vector.
#'
#' @param x A vector.
#' @return The softmax of `x`
softmax <- function(x) {
  exp(x) / sum(exp(x))
}

#' Compute the numerically-stable softmax of a vector.
#'
#' @param x A vector.
#' @return The softmax of `x`
stable_softmax <- function(x) {
  x <- x - max(x)
  exp(x) / sum(exp(x))
}

#' Categorical to indicators (dummy variables).
#'
#' @param x A categorical column.
#' @return Indicator variables column.
to_int <- function(x) {
  as.integer(as.factor(x))
}

#' Fixing the precision prior in `R-INLA`.
#'
#' @param x Used to specify the `initial`.
#' @return A prior that can be passed to `R-INLA`.
tau_fixed <- function(x) {
  list(prec = list(initial = log(x), fixed = TRUE))
}

#' Penalised complexity precision prior for `R-INLA`.
#'
#' @param x Used to specify the `initial`.
#' @param u Upper threshold.
#' @param alpha Probability that the standard deviation exceeds
#' the upper threshold.
#' @return A prior that can be passed to `R-INLA`.
tau_pc <- function(x, u, alpha) {
  list(prec = list(prec = "pc.prec", param = c(u, alpha), initial = log(x)))
}

#' Fit multinomial model using Poisson trick.
#'
#' @param formula A formula object passed to `R-INLA`.
#' @param model A string containing the name of the model.
#' @param S The number of Monte Carlo samples used in posterior predictive checks.
#' @return A dataframe adding columns to `df_model`.
multinomial_model <- function(formula, model_name, S = 1000) {

  message(paste0("Begin fitting ", model_name, "."))

  fit <- inla(formula, data = df_model, family = 'xPoisson',
              control.predictor = list(link = 1),
              control.compute = list(dic = TRUE, waic = TRUE,
                                     cpo = TRUE, config = TRUE))

  df_model <- df_model %>%
    mutate(
      #' Add mean of linear predictor
      eta = fit$summary.linear.predictor$mean
    ) %>%
    #' Split by observation indicator and lapply softmax
    split(.$obs_idx) %>%
    lapply(function(x)
      x %>%
        mutate(prob_mean = stable_softmax(eta))
    ) %>%
    bind_rows() %>%
    #' Remove eta
    select(-eta) %>%
    #' Add model identifier
    mutate(model = model_name)

  #' Number of samples from the posterior, keep it low to begin with
  full_samples <- inla.posterior.sample(n = S, result = fit)

  #' Calculate the probabilities for each sample from the posterior
  x <- lapply(
    seq_along(full_samples),
    function(i)
      full_samples[[i]]$latent %>%
      data.frame() %>%
      tibble::rownames_to_column() %>%
      #' eta = 2 is the second column, which usually is called
      #' paste0("sample.", i) but I have experienced some inconsistency
      #' from this within INLA so avoiding
      rename(eta = 2) %>%
      filter(substr(rowname, 1, 10) == "Predictor:") %>%
      bind_cols(
        df_model %>%
          select(age_idx, area_idx, obs_idx, cat_idx, n_eff_kish, population_mean)
      ) %>%
      split(.$obs_idx) %>%
      lapply(function(x)
        x %>%
          mutate(
            #' Use stable_softmax(eta) here as the probability with additional multinomial sampling variability
            #' Would like to sample from multinomial with non-integer counts for n_eff_kish but can't
            #' Instead use the floor, which the size argument automatically does but written here for clarity
            #' Sometimes n_eff_kish is NA (missing data). In these cases we use 100 (around the average)
            #' This is a little makeshift but probably sufficient for our purposes
            prob = ifelse(!is.na(n_eff_kish),
                          stats::rmultinom(n = 1, size = floor(n_eff_kish), prob = stable_softmax(eta)) / n_eff_kish,
                          stats::rmultinom(n = 1, size = 100, prob = stable_softmax(eta)) / 100
            )
          )
      ) %>%
      bind_rows() %>%
      mutate(
        #' Sample of the intensity
        lambda = exp(eta),
        #' Sample number / identifier
        sample = i
      )
  ) %>%
    bind_rows()

  #' Obtain quantiles from the inla.posterior.sample and join them into df_model
  df_model <- df_model %>%
    left_join(
      left_join(x, df_model, by = c("obs_idx", "cat_idx")) %>%
        group_by(obs_idx, cat_idx) %>%
        summarise(
          #' The quantile of the raw estimate
          estimate = mean(estimate), #' These should all be identical anyway
          prob_quantile = ecdf(prob)(estimate),
          #' Quantiles of the proportion
          prob_median = quantile(prob, 0.5, na.rm = TRUE),
          prob_lower = quantile(prob, 0.025, na.rm = TRUE),
          prob_upper = quantile(prob, 0.975, na.rm = TRUE),
          #' Quantiles of the intensity, used to calculate sample size recovery later
          lambda_median = quantile(lambda, 0.5, na.rm = TRUE),
          lambda_lower = quantile(lambda, 0.025, na.rm = TRUE),
          lambda_upper = quantile(lambda, 0.975, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        select(-estimate),
      by = c("obs_idx", "cat_idx")
    )

  #' Maybe temporary fix (along with na.rm = TRUE) for when some of the samples are NaN
  #' Should be investigated further
  if(sum(is.na(x$prob)) > 0) {
    message(
      paste0(
        "Warning: ", sum(is.na(x$prob)), " of the ", sum(!is.na(x$prob)),
        " Monte Carlo probability samples generated by inla.posterior.sample were NaN!"
      )
    )
  }

  #' Adding in aggregates
  mean_aggregate_Y015_024 <- df_model %>%
    #' The 15-19 and 20-24 age groups
    filter(age_idx %in% c(1, 2)) %>%
    group_by(area_idx, cat_idx) %>%
    summarise(prob_mean = sum(prob_mean * population_mean) / sum(population_mean), .groups = "drop")

  quantiles_aggregate_Y015_024 <- x %>%
    #' The 15-19 and 20-24 age groups
    filter(age_idx %in% c(1, 2)) %>%
    group_by(area_idx, cat_idx, sample) %>%
    #' prob = sum_i(prob_i * pop_i) / sum_i(pop_i)
    summarise(prob = sum(prob_mean * population_mean) / sum(population_mean),
              .groups = "drop") %>%
    group_by(area_idx, cat_idx) %>%
    summarise(prob_median = quantile(prob, 0.5, na.rm = TRUE),
              prob_lower = quantile(prob, 0.025, na.rm = TRUE),
              prob_upper = quantile(prob, 0.975, na.rm = TRUE),
              .groups = "drop")

  mean_aggregate_national <- df_model %>%
    group_by(age_idx, cat_idx) %>%
    summarise(prob_mean = sum(prob_mean * population_mean) / sum(population_mean), .groups = "drop")

  quantiles_aggregate_national <- x %>%
    group_by(age_idx, cat_idx, sample) %>%
    #' prob = sum_i(prob_i * pop_i) / sum_i(pop_i)
    summarise(prob = sum(prob * population_mean) / sum(population_mean),
              .groups = "drop") %>%
    group_by(age_idx, cat_idx) %>%
    summarise(prob_median = quantile(prob, 0.5, na.rm = TRUE),
              prob_lower = quantile(prob, 0.025, na.rm = TRUE),
              prob_upper = quantile(prob, 0.975, na.rm = TRUE),
              .groups = "drop")

  #' TODO: There are still NA in the intersection of 15-24 and country
  df_agg <- df_agg %>%
    mutate(model = model_name) %>%
    #' The mean of 15-24 age group aggregate measures
    left_join(mean_aggregate_Y015_024, by = c("area_idx", "cat_idx")) %>%
    #' The mean of the national aggregate measures
    left_join(mean_aggregate_national, by = c("age_idx", "cat_idx")) %>%
    #' Overwriting NAs left_join
    within(., prob_mean.x <- ifelse(!is.na(prob_mean.y), prob_mean.y, prob_mean.x)) %>%
    select(-prob_mean.y) %>%
    rename(prob_mean = prob_mean.x) %>%
    #' The quantiles of 15-24 age group aggregate measures
    left_join(quantiles_aggregate_Y015_024, by = c("area_idx", "cat_idx")) %>%
    #' The quantiles of the national aggregate measures
    left_join(quantiles_aggregate_national, by = c("age_idx", "cat_idx")) %>%
    #' Overwriting NAs left_join
    within(., {
      prob_median.x <- ifelse(!is.na(prob_median.y), prob_median.y, prob_median.x)
      prob_lower.x <- ifelse(!is.na(prob_lower.y), prob_lower.y, prob_lower.x)
      prob_upper.x <- ifelse(!is.na(prob_upper.y), prob_upper.y, prob_upper.x)
    }) %>%
    select(-prob_median.y, -prob_lower.y, -prob_upper.y) %>%
    rename(prob_median = prob_median.x, prob_lower = prob_lower.x, prob_upper = prob_upper.x)

  message(paste0("Completed fitting ", model_name, "."))

  #' df goes back to including the aggregate group here, perhaps it's confusing to do this!
  return(list(df = bind_rows(df_model, df_agg), fit = fit))
}


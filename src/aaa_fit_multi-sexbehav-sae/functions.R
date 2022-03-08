#' Fit multinomial model using Poisson trick.
#'
#' @param formula A formula object passed to `R-INLA`.
#' @param model A string containing the name of the model.
#' @param S The number of Monte Carlo samples used in posterior predictive checks.
#' @return A dataframe adding columns to `df`, the fitted model object `fit`,
#' and samples from the fitted model object `samples`.
multinomial_model <- function(formula, model_name, S = 1000) {

  message(paste0("Begin fitting ", model_name, "."))

  fit <- inla(formula, data = df, family = 'xPoisson',
              control.predictor = list(link = 1),
              control.compute = list(dic = TRUE, waic = TRUE,
                                     cpo = TRUE, config = TRUE),
              inla.mode = "experimental")

  message(paste0("Completed fitting ", model_name, "."))

  message("Begin post-processing")

  df <- df %>%
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
        df %>%
          #' c(obs_idx, cat_idx) is sufficient to identify
          select(obs_idx, cat_idx, n_eff_kish)
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
      ) %>%
      #' Add model identifier
      mutate(model = model_name)
  ) %>%
    bind_rows()

  #' Obtain quantiles from the inla.posterior.sample and join them into df
  df <- df %>%
    left_join(
      left_join(x, df, by = c("obs_idx", "cat_idx")) %>%
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

  message("Completed post-processing")

  return(list(df = df, fit = fit, samples = x))
}

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

  #' Full R-INLA samples
  full_samples <- inla.posterior.sample(n = S, result = fit)

  #' Just the latent field
  eta_samples <- lapply(full_samples, "[", "latent")

  #' For some reason "latent" is comprised of more than only the latent field
  eta_samples <- lapply(eta_samples, function(eta_sample) {
    data.frame(eta_sample) %>%
      tibble::rownames_to_column() %>%
      rename(eta = 2) %>%
      filter(substr(rowname, 1, 10) == "Predictor:") %>%
      select(-rowname)
  })

  #' Into a matrix with a row for each observation and a column for each sample
  eta_samples_matrix <- matrix(unlist(eta_samples), ncol = S)
  eta_samples_df <- data.frame(eta_samples_matrix)

  samples <- eta_samples_df  %>%
    mutate(
      #' To split by
      obs_idx = df$obs_idx,
      #' To sample predictive
      n_eff_kish_new = floor(ifelse(is.na(df$n_eff_kish), 100, df$n_eff_kish))
    ) %>%
    split(.$obs_idx) %>%
    mclapply(function(x) {
      n_eff_kish_new <- x$n_eff_kish_new
      #' Remove the obs_idx and n_eff_kish_new columns
      x_samples <- x[1:(length(x) - 2)]
      #' Normalise each column (to avoid overflow of softmax)
      x_samples <- apply(x_samples, MARGIN = 2, FUN = function(x) x - max(x))
      #' Exponentiate (can be done outside apply)
      lambda_samples <- exp(x_samples)
      #' Calculate samples from posterior of probabilites
      prob_samples <- apply(lambda_samples, MARGIN = 2, FUN = function(x) x / sum(x))
      #' Calculate predictive samples (including sampling variability)
      prob_predictive_samples <- apply(prob_samples, MARGIN = 2, FUN = function(x) {
        stats::rmultinom(n = 1, size = n_eff_kish_new, prob = x) / n_eff_kish_new
      })
      #' Return list, allowing extraction of each set of samples
      list(
        lambda = data.frame(lambda_samples),
        prob = data.frame(prob_samples),
        prob_predictive = data.frame(prob_predictive_samples))
    })

  lambda_samples_df <- bind_rows(lapply(samples, "[[", "lambda"))
  prob_samples_df <- bind_rows(lapply(samples, "[[", "prob"))
  prob_predictive_samples_df <- bind_rows(lapply(samples, "[[", "prob_predictive"))

  #' Helper functions
  row_summary <- function(df, ...) unname(apply(df, MARGIN = 1, ...))
  median <- function(x) quantile(x, 0.5, na.rm = TRUE)
  lower <- function(x) quantile(x, 0.025, na.rm = TRUE)
  upper <- function(x) quantile(x, 0.975, na.rm = TRUE)

  #' Calculate mean, median, lower and upper for each set of samples
  df <- df %>%
    mutate(
      lambda_mean = row_summary(lambda_samples_df, mean),
      lambda_median = row_summary(lambda_samples_df, median),
      lambda_lower = row_summary(lambda_samples_df, lower),
      lambda_upper = row_summary(lambda_samples_df, upper),
      prob_mean = row_summary(prob_samples_df, mean),
      prob_median = row_summary(prob_samples_df, median),
      prob_lower = row_summary(prob_samples_df, lower),
      prob_upper = row_summary(prob_samples_df, upper),
      prob_predictive_mean = row_summary(prob_predictive_samples_df, mean),
      prob_predictive_median = row_summary(prob_predictive_samples_df, median),
      prob_predictive_lower = row_summary(prob_predictive_samples_df, lower),
      prob_predictive_upper = row_summary(prob_predictive_samples_df, upper),
      model = model_name
    )

  message("Completed post-processing")

  return(list(df = df, fit = fit))
}

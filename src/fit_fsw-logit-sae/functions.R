#' Fit logistic regression model.
#'
#' @param formula A formula object passed to `R-INLA`.
#' @return A fitted model object.
logistic_model <- function(formula, model_name, S = 1000) {

  message(paste0("Begin fitting ", model_name, "."))

  fit <- inla(
    formula,
    data = df,
    family = 'xbinomial',
    Ntrials = n_eff_kish,
    control.family = list(control.link = list(model = "logit")),
    control.predictor = list(compute = TRUE, link = 1),
    control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, config = TRUE),
    inla.mode = "experimental"
  )

  df <- df %>%
    mutate(
      prob_mean = fit$summary.fitted.values$mean,
      prob_median = fit$summary.fitted.values$`0.5quant`,
      prob_lower = fit$summary.fitted.values$`0.025quant`,
      prob_upper = fit$summary.fitted.values$`0.975quant`,
      model = model_name
    )

  full_samples <- inla.posterior.sample(n = S, result = fit)

  message(paste0("Completed fitting ", model_name, "."))

  return(list(df = df, fit = fit, samples = full_samples))
}

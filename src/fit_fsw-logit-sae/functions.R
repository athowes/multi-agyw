#' Fit logistic regression model.
#'
#' @param formula A formula object passed to `R-INLA`.
#' @return A fitted model object.
logistic_model <- function(formula, model_name, S = 1000) {
  fit <- inla(
    formula,
    data = df_model,
    family = 'xbinomial',
    Ntrials = n_eff_kish,
    control.family = list(control.link = list(model = "logit")),
    control.predictor = list(compute = TRUE, link = 1),
    control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, config = TRUE),
    inla.mode = "experimental"
  )

  df_model <- df_model %>%
    mutate(
      prob_mean = fit$summary.fitted.values$mean,
      prob_median = fit$summary.fitted.values$`0.5quant`,
      prob_lower = fit$summary.fitted.values$`0.025quant`,
      prob_upper = fit$summary.fitted.values$`0.975quant`,
      model = model_name
    )

  full_samples <- inla.posterior.sample(n = S, result = fit)

  return(list(df = df_model, fit = fit, samples = full_samples))
}

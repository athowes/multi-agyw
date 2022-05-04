#' Fit logistic regression model.
#'
#' @param formula A formula object passed to `R-INLA`.
#' @return A fitted model object.
logistic_model <- function(formula, model_name) {

  message(paste0("Begin fitting ", model_name, "."))

  prior_fixed <- list(
    mean.intercept = -2,
    prec.intercept = 1,
    mean = 0,
    prec = 0.16
  )

  fit <- inla(
    formula,
    data = df,
    family = 'xbinomial',
    Ntrials = n_eff_kish,
    control.family = list(control.link = list(model = "logit")),
    control.predictor = list(compute = TRUE, link = 1),
    control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, config = TRUE),
    control.fixed = prior_fixed,
    inla.mode = "experimental"
  )

  message(paste0("Completed fitting ", model_name, "."))

  message("Begin post-processing")

  df <- df %>%
    mutate(
      prob_mean = fit$summary.fitted.values$mean,
      prob_median = fit$summary.fitted.values$`0.5quant`,
      prob_lower = fit$summary.fitted.values$`0.025quant`,
      prob_upper = fit$summary.fitted.values$`0.975quant`,
      model = model_name
    )

  message("Completed post-processing")

  return(list(df = df, fit = fit))
}

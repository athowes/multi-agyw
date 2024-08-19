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

  message("Skipping post-processing")

  return(list(fit = fit))
}

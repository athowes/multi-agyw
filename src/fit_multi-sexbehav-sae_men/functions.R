#' Fit multinomial model using Poisson trick.
#'
#' @param formula A formula object passed to `R-INLA`.
#' @param model A string containing the name of the model.
#' @return A dataframe adding columns to `df`.
multinomial_model <- function(formula, model_name) {

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

  message("Completed post-processing")

  return(list(df = df, fit = fit))
}

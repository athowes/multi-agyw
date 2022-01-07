#' Constrain vector to be inside interval
#'
#' @param x A vector.
#' @param lower The smallest possible value of `x`.
#' @param upper The largest possible value of `x`.
#' @return A vector `x` where values outside the interval are moved inside.
constrain_interval <- function(x, lower, upper, verbose = TRUE) {
  if(verbose) {
    message(
      paste0(
        "There are: ", sum(x < lower), " values < ", lower, " and ", sum(x > upper),
        " values > ", upper, ". If they exist, these values have been set to be inside [0, 1]!"
      )
    )
  }
  x <- pmin(1, x)
  x <- pmax(0, x)
  x
}

#' Categorical to indicators.
#'
#' @param x A categorical column.
#' @return Indicator variables column.
to_int <- function(x) {
  as.integer(as.factor(x))
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

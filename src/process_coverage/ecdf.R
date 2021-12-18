#' Calculates the empirical coverage of a vector `x`.
empirical_coverage <- function(x, nominal_coverage) {
  alpha <- (1 - nominal_coverage)
  y <- (x >= (alpha / 2)) & (x <= 1 - (alpha / 2))
  sum(y) / length(y)
}

#' This function simulates upper and lower limits `lims$lower` and `lims$upper` such that:
#' \deqn{P({lims$lower[i] <= sum(unif(n) < 1/i) <= lims$upper[i]} for any i = 1, \ldots, K - 1) = 1 - alpha}
#'
#' For more information see "Graphical Test for Discrete Uniformity and its Applications in
#' Goodness of Fit Evaluation and Multiple Sample Comparison" (Säilynoja, Bürkner, Vehtari).
#'
#' @param n Sample size.
#' @param alpha Confidence level between 0 and 1.
#' @param K Granularity of the uniform partition of the unit interval.
#' @return List containing the upper and lower simultaneous confidence bands
#' evaluated at \eqn{z_i = i / K} for \eqn{i = 1, \ldots, K - 1}.
#' @source https://github.com/TeemuSailynoja/simultaneous-confidence-bands
get_lims <- function(alpha, n, K) {
  gamma <- adjust_alpha_optimize(alpha, n, K) #' Compute coverage parameter gamma
  lims <- list(
    lower = qbinom(gamma / 2, n, (0:K) / K),
    upper = qbinom(1 - gamma / 2, n, (0:K) / K)
  )
}

#' Adjust the alpha level of the envelope test using optimization
#' @source https://github.com/TeemuSailynoja/simultaneous-confidence-bands
adjust_alpha_optimize <- function(alpha, N, K) {
  optimize(target_gamma, c(0, alpha), alpha = alpha, N = N, K = K)$minimum
}

#' @source https://github.com/TeemuSailynoja/simultaneous-confidence-bands
target_gamma <- function(gamma, alpha, N, K) {
  z <- 1:(K - 1) / K
  z2 <- c(z, 1)
  z1 <- c(0, z)

  #' Pre-compute quantiles and use symmetry for increased efficiency
  x2_lower <- qbinom(gamma / 2, N, z2)
  x2_upper <- c(N - rev(x2_lower)[seq_len(K)[-1]], N)

  #' Compute the total probability of trajectories inside the envelope
  x1 <- 0
  p_int <- 1
  for (i in seq_along(z1)) {
    tmp <- p_interior(
      p_int, x1 = x1, x2 = x2_lower[i]:x2_upper[i],
      z1 = z1[i], z2 = z2[i], gamma = gamma, N = N
    )
    x1 <- tmp$x1
    p_int <- tmp$p_int
  }
  abs(1 - alpha - sum(p_int))
}

#' Probability of the ECDF being completely within the envelope as z2
#' @source https://github.com/TeemuSailynoja/simultaneous-confidence-bands
p_interior <- function(p_int, x1, x2, z1, z2, gamma, N) {
  z_tilde <- (z2 - z1) / (1 - z1)

  N_tilde <- rep(N - x1, each = length(x2))
  p_int <- rep(p_int, each = length(x2))
  x_diff <- outer(x2, x1, "-")
  p_x2_int <- p_int * dbinom(x_diff, N_tilde, z_tilde)

  list(p_int = rowSums(p_x2_int), x1 = x2)
}

#' Suppose that the ORs we calculate are
or <- c(1, 3.8, 3.7, 5.1)

#' And in log-odds ratio form
lor <- log(or)

#' 10% prevalence
prev <- 0.1

#' 100 people in the district
N <- 100

#' Broken down by behaviour as follows
N_fine <- c(30, 20, 45, 5)

#' @param lor Log odds-ratios
#' @param N_fine Number of individuals in each group
#' @param prev Prevalence in
#' @param N
logit_scale_prev <- function(lor, N_fine, prev, N) {
  #' Total number of PLHIV
  target <- prev * N
  #' theta represents prevalence in baseline risk group
  #' plogis(lor + theta) is prevalence in each risk group
  #' plogis(lor + theta) * N_fine is PLHIV in each risk group
  optfn <- function(theta) (sum(plogis(lor + theta) * N_fine) - target)^2
  #' Optimisation for baseline risk group prevalence
  #' On the logit scale should be more numerically stable
  opt <- optimise(optfn, c(-10, 10), tol = .Machine$double.eps^0.5)
  #' Return prevalence
  plogis(lor + opt$minimum)
}

logit_scale_prev(lor, N_fine, prev, N)

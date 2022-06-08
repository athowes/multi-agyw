#' Calculate the odds
#'
#' @param p Probability in [0, 1]
odds <- function(p) p / (1 - p)

#' Calculate YWKP prevalence ratio and log odds ratio
#'
#' @param prev (General population) prevalence
#' @param fit A model relating log-odds prevalence to YWKP log odds prevalence
calculate_ywkp_pr_lor <- function(prev, fit = ywkp_fit) {
  prev_logodds <- qlogis(prev)
  prev_ywkp_logodds <- predict(fit, data.frame(prev_logodds = prev_logodds))
  prev_ywkp <- plogis(prev_ywkp_logodds)

  #' Prevalence ratio
  pr <- prev_ywkp / prev

  #' Fix it so that the PR is in [2.5, 20], and any zero prevalence PRs are set to 1
  prev_ywkp[pr < 2.5] <- 2.5 * prev[pr < 2.5]
  prev_ywkp[pr > 20] <- 20 * prev_ywkp[pr > 20]
  pr[prev == 0] <- 1
  prev_ywkp[prev_ywkp > 1] <- 1
  pr <- prev_ywkp / prev

  #' Log odds-ratio (fixed so that the PR is in [2.5, 20] as above)
  lor <- log(odds(prev_ywkp) / odds(prev))

  return(list(pr = pr, lor = lor, prev = prev, prev_ywkp = prev_ywkp))
}

#' Calculate prevalence and PLHIV using logit-scale disaggregation
#'
#' @param lor Log odds-ratios
#' @param N_fine Number of individuals in each group
#' @param plhiv Total number of people living with HIV
logit_scale_prev <- function(lor, N_fine, plhiv) {
  #' theta represents prevalence in baseline risk group
  #' plogis(lor + theta) is prevalence in each risk group
  #' plogis(lor + theta) * N_fine is PLHIV in each risk group
  optfn <- function(theta) (sum(plogis(lor + theta) * N_fine) - plhiv)^2
  #' Optimisation for baseline risk group prevalence
  #' On the logit scale should be more numerically stable
  opt <- optimise(optfn, c(-10, 10), tol = .Machine$double.eps^0.5)
  #' Return prevalence
  plogis(lor + opt$minimum)
}

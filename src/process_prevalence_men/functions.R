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
  #' Ensure that the LOR is above that of e.g. the sexnonreg risk group
  prev_ywkp_logodds <- pmax(prev_ywkp_logodds, prev_logodds + 0.25)
  prev_ywkp <- plogis(prev_ywkp_logodds)
  #' Prevalence ratio
  pr <- prev_ywkp / prev
  #' Log-odds ratio
  lor <- prev_ywkp_logodds - prev_logodds
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

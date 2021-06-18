#' Uncomment and run the two line below to resume development of this script
orderly::orderly_develop_start("dev_multi-sexbehav-sae")
setwd("src/dev_multi-sexbehav-sae/")

#' Add age specific identifier
df <- read_csv("depends/simulated-sexbehav.csv")

#' See Serafini's "Multinomial logit models with INLA" tutorial
#' https://inla.r-inla-download.org/r-inla.org/doc/vignettes/multinomial.pdf

#' Model 1.
#' No intercept (-1) and observation specific intercepts f(obs_idx, ...)
formula1 <- y ~ -1 + f(obs_idx, hyper = list(prec = list(initial = log(0.000001), fixed = TRUE)))

fit1 <- inla(formula1, data = df, family = "Poisson",
             control.predictor = list(link = 1),
             control.compute = list(config = TRUE))

#' These are the same. So to what extent is this just the same as an intercept?
max(fit1$summary.random$obs_idx$mean)
min(fit1$summary.random$obs_idx$mean)

formula0 <- y ~ 1

fit0 <- inla(formula0, data = df, family = "Poisson",
             control.predictor = list(link = 1),
             control.compute = list(config = TRUE))

#' This is the same as fit1$summary.random$obs_idx$mean
fit0$summary.fixed

#' Model 2.
#' Add category specific random effects
formula2 <- y ~ -1 + f(obs_idx, hyper = list(prec = list(initial = log(0.000001), fixed = TRUE))) +
  f(cat_idx, model = "iid", constr = TRUE, hyper = list(prec = list(initial = log(0.001), fixed = TRUE)))

fit2 <- inla(formula2, data = df, family = "Poisson",
             control.predictor = list(link = 1),
             control.compute = list(config = TRUE))

#' Still all essentially the same
hist(fit2$summary.random$obs_idx$mean)

#' The category random effects give different intercept for each category
etas <- fit2$summary.linear.predictor$mean[1:4]

#' From this can get the fitted category probabilities (same for each district-age)
#' Compare to truth from sim_sexbehav. Pretty close
prob <- matrix(
  c(0.6, 0.25, 0.12, 0.03,
    0.2, 0.4, 0.35, 0.05,
    0.05, 0.5, 0.4, 0.05),
  byrow = TRUE,
  ncol = 4
)

rownames(prob) <- c("Y015_019", "Y020_024", "Y025_029")
colnames(prob) <- c("nosex12m", "sexcohab", "sexnonreg", "sexpaid12m")
cbind(truth = colMeans(prob), pred = exp(etas) / sum(exp(etas)))

#' Model 3.
#' Add age specific random effects

df <- mutate(df, age_idx = case_when(
  age_group == "Y015_019" ~ 1,
  age_group == "Y020_024" ~ 2,
  age_group == "Y025_029" ~ 3)
)

formula3 <- y ~ -1 + f(obs_idx, hyper = list(prec = list(initial = log(0.000001), fixed = TRUE))) +
  f(cat_idx, model = "iid", constr = TRUE, hyper = list(prec = list(initial = log(0.001), fixed = TRUE))) +
  f(age_idx, model = "iid", constr = TRUE, hyper = list(prec = list(initial = log(0.001), fixed = TRUE)))

fit3 <- inla(formula3, data = df, family = "Poisson",
             control.predictor = list(link = 1),
             control.compute = list(config = TRUE))

#' The first indexes for 15-19 are 1:4, 20-24 are 6601:6604 and 25-29 are 13201:13204

min(which(df$age_idx == 2))
min(which(df$age_idx == 3))

calculate_pred <- function(fit) {
  etas_Y015_019 <- fit$summary.linear.predictor$mean[1:4]
  etas_Y020_024 <-fit$summary.linear.predictor$mean[6601:6604]
  etas_Y025_029 <-fit$summary.linear.predictor$mean[13201:13204]

  pred <- t(sapply(
    list(etas_Y015_019, etas_Y020_024, etas_Y025_029),
    function(x) exp(x) / sum(exp(x))
  ))

  rownames(pred) <- c("Y015_019", "Y020_024", "Y025_029")
  colnames(pred) <- c("nosex12m", "sexcohab", "sexnonreg", "sexpaid12m")

  return(pred)
}

#' Getting this wrong still!
list(truth = prob, pred = calculate_pred(fit3))

#' The category specific random effects are the same as in Model 2
fit3$summary.random$cat_idx
fit2$summary.random$cat_idx

#' The age specific random effects are approximately zero
fit3$summary.random$age_idx

#' The empirical probabilities show it's not a problem with the simulated data
df %>%
  group_by(age_group, cat_idx) %>%
  summarise(y_mean = mean(y))

#' The problem is that
#' exp(eta + c) / sum(exp(eta + c)) = exp(eta) / exp
#' For example
exp(etas) / sum(exp(etas))
exp(etas + 1) / sum(exp(etas + 1))

#' Model 4.
#' Age x category interactions rather than age random effects

df <- mutate(df, age_cat_idx = interaction(age_idx, cat_idx))

formula4 <- y ~ -1 + f(obs_idx, hyper = list(prec = list(initial = log(0.000001), fixed = TRUE))) +
  f(cat_idx, model = "iid", constr = TRUE, hyper = list(prec = list(initial = log(0.001), fixed = TRUE))) +
  f(age_cat_idx, model = "iid", constr = TRUE, hyper = list(prec = list(initial = log(0.001), fixed = TRUE)))

fit4 <- inla(formula4, data = df, family = "Poisson",
             control.predictor = list(link = 1),
             control.compute = list(config = TRUE))

#' Got it!
list(truth = prob, pred = calculate_pred(fit4))

#' Model 5.
#' Kinh's version of the interaction

formula5 <- y ~ -1 + f(obs_idx, hyper = list(prec = list(initial = log(0.000001), fixed = TRUE))) +
  f(cat_idx, age_idx, model = "iid", constr = TRUE, hyper = list(prec = list(initial = log(0.001), fixed = TRUE)))

fit5 <- inla(formula5, data = df, family = "Poisson",
             control.predictor = list(link = 1),
             control.compute = list(config = TRUE))

#' Appears to be doing something different to Model 4.
fit5$summary.random
list(truth = prob, pred = calculate_pred(fit5))

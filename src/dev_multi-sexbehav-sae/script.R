#' Uncomment and run the two line below to resume development of this script
orderly::orderly_develop_start("dev_multi-sexbehav-sae")
setwd("src/dev_multi-sexbehav-sae/")

df <- read_csv("depends/simulated-sexbehav.csv")

#' See Serafini's "Multinomial logit models with INLA" tutorial
#' https://inla.r-inla-download.org/r-inla.org/doc/vignettes/multinomial.pdf

#' Model 1.
#' No intercept (-1) and observation specific intercepts f(obs_idx, ...)
formula1 <- y ~ -1 + f(obs_idx, initial = -10, fixed = TRUE)

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
formula2 <- y ~ -1 + f(obs_idx, initial = -10, fixed = TRUE) +
  f(cat_idx, model = "iid")

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
cbind(truth = colMeans(prob), pred = exp(etas) / sum(exp(etas)))

#' Model 3.
#' Add age specific random effects
#' Though what is required are age category interactions

#' Start by adding age specific identifier
df <- mutate(df, age_idx = case_when(
  age_group == "Y015_019" ~ 1,
  age_group == "Y020_024" ~ 2,
  age_group == "Y025_029" ~ 3
))

formula3 <- y ~ -1 + f(obs_idx, initial = -10, fixed = TRUE) +
  f(cat_idx, model = "iid") +
  f(age_idx, model = "iid")

fit3 <- inla(formula3, data = df, family = "Poisson",
             control.predictor = list(link = 1),
             control.compute = list(config = TRUE))

#' The first indexes for 15-19 are 1:4, 20-24 are 133:136 and 25-29 are 265:268
#' But for some reason the linear predictors are all the same
fit3$summary.linear.predictor$mean[1:4]
fit3$summary.linear.predictor$mean[133:136]
fit3$summary.linear.predictor$mean[265:268]

#' The category specific random effects are the same as in Model 2
fit3$summary.random$cat_idx
fit2$summary.random$cat_idx

#' The age specific random effects are approximately zero
#' Don't know why this is. Identifiability issues?
fit3$summary.random$age_idx

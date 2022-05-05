#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("process_sampling")
# setwd("src/process_sampling")

#' Start with very low number of samples
S <- 4

#' Multinomial regression model
fits <- readRDS("depends/multi-sexbehav-sae-fits.rds")
fit <- fits[[1]] #' Because it's a lightweight run
full_samples <- inla.posterior.sample(n = S, result = fit)
saveRDS(full_samples, "multi-sexbehav-sae-samples.rds")

#' Logistic regression model
fit_prop <- readRDS("depends/best-fsw-logit-sae-fit.rds")
full_samples_prop <- inla.posterior.sample(n = S, result = fit_prop)
saveRDS(full_samples_prop, "fsw-logit-sae-samples.rds")

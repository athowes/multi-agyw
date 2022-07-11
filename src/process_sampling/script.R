#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("process_sampling")
# setwd("src/process_sampling")

#' In testing, 100 samples can be used
#' For the final results, 1000 samples should be used
S <- 1000

#' Multinomial regression model
fit <- readRDS("depends/best-multi-sexbehav-sae-fit.rds")
full_samples <- inla.posterior.sample(n = S, result = fit)
saveRDS(full_samples, "multi-sexbehav-sae-samples.rds")

#' Logistic regression model
fit_prop <- readRDS("depends/best-fsw-logit-sae-fit.rds")
full_samples_prop <- inla.posterior.sample(n = S, result = fit_prop)
saveRDS(full_samples_prop, "fsw-logit-sae-samples.rds")

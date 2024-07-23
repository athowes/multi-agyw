#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("process_sampling_men")
# setwd("src/process_sampling_men")

#' In testing, 100 samples can be used
#' For the final results, 1000 samples should be used
S <- 1000

#' Multinomial regression model
fit <- readRDS("depends/best-multi-sexbehav-sae-fit.rds")

eta_samples <- list()

for(i in 1:200) {
  print(i)
  samples_sub <- inla.posterior.sample(n = S/200, result = fit)
  #' Just the latent field
  eta_samples_sub <- lapply(samples_sub, "[", "latent")

  # very big file!!
  rm(samples_sub)

  #' For some reason "latent" is comprised of more than only the latent field
  eta_samples <- c(eta_samples, lapply(eta_samples_sub, function(eta_sample) {
    data.frame(eta_sample) %>%
      tibble::rownames_to_column() %>%
      rename(eta = 2) %>%
      filter(substr(rowname, 1, 10) == "Predictor:") %>%
      select(-rowname)
  }))
  rm(eta_samples_sub)
}

saveRDS(eta_samples, "multi-sexbehav-sae-samples.rds")

#' Logistic regression model
# fit_prop <- readRDS("depends/best-fsw-logit-sae-fit.rds")
# full_samples_prop <- inla.posterior.sample(n = S, result = fit_prop)
# saveRDS(full_samples_prop, "fsw-logit-sae-samples.rds")

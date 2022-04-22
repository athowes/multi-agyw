#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("process_differentiate-high-risk")
# setwd("src/process_differentiate-high-risk")

df_3_aaa <- read_csv("depends/best-3-aaa-multi-sexbehav-sae.csv")
df_3 <- read_csv("depends/multi-sexbehav-sae.csv") %>%
  filter(model == "Model 4") #' Temporary solution, should be earlier in pipeline

#' Just want one set of estimates for each country
df_prop <- read_csv("depends/best-fsw-logit-sae.csv") %>%
  filter(!(survey_id %in% c("MWI2015DHS", "ZMB2016PHIA", "ZWE2015DHS")))

df_3p1_aaa <- differentiate_high_risk(df_3_aaa, df_prop)
write_csv(df_3p1_aaa, "best-3p1-aaa-multi-sexbehav-sae.csv", na = "")

df_3p1 <- differentiate_high_risk(df_3, df_prop)
write_csv(df_3p1, "best-3p1-multi-sexbehav-sae.csv", na = "")

#' And now on the samples
fits <- readRDS("depends/multi-sexbehav-sae-fits.rds")
fit <- fits[[1]]
samples_prop <- readRDS("depends/best-fsw-logit-sae-samples.rds")

#' Start with very low number of samples
S <- 4
full_samples <- inla.posterior.sample(n = S, result = fit)

#' Just the latent field
eta_samples <- lapply(full_samples, "[", "latent")

#' For some reason "latent" is comprised of more than only the latent field
eta_samples <- lapply(eta_samples, function(eta_sample) {
  data.frame(eta_sample) %>%
    tibble::rownames_to_column() %>%
    rename(eta = 2) %>%
    filter(substr(rowname, 1, 10) == "Predictor:") %>%
    select(-rowname)
})

#' Into a matrix with a row for each observation and a column for each sample
eta_samples_matrix <- matrix(unlist(eta_samples), ncol = S)
eta_samples_df <- data.frame(eta_samples_matrix)

cores <- detectCores()
ncores <- 4

start_time <- Sys.time()

samples <- eta_samples_df  %>%
  mutate(
    #' To split by
    obs_idx = df_3$obs_idx,
    #' To sample predictive
    #' When n_eff_kish is missing there is no survey for that observation,
    #' so the posterior predictive is meaningless. Setting to zero may save
    #' some computation, but probably better to filter out entirely.
    n_eff_kish_new = floor(ifelse(is.na(df_3$n_eff_kish), 0, df_3$n_eff_kish))
  ) %>%
  split(.$obs_idx) %>%
  mclapply(function(x) {
    n_eff_kish_new <- x[["n_eff_kish_new"]]
    #' Remove the obs_idx and n_eff_kish_new columns
    x_samples <- x[1:(length(x) - 2)]
    #' Normalise each column (to avoid overflow of softmax)
    x_samples <- apply(x_samples, MARGIN = 2, FUN = function(x) x - max(x))
    #' Exponentiate (can be done outside apply)
    #' WARNING: That these are samples from lambda posterior isn't true! Come back to this
    lambda_samples <- exp(x_samples)
    #' Calculate samples from posterior of probabilities
    prob_samples <- apply(lambda_samples, MARGIN = 2, FUN = function(x) x / sum(x))
    #' Calculate predictive samples (including sampling variability)
    prob_predictive_samples <- apply(prob_samples, MARGIN = 2, FUN = function(x) {
      stats::rmultinom(n = 1, size = n_eff_kish_new, prob = x) / n_eff_kish_new
    })
    #' Return list, allowing extraction of each set of samples
    list(
      lambda = data.frame(lambda_samples),
      prob = data.frame(prob_samples),
      prob_predictive = data.frame(prob_predictive_samples))
  }, mc.cores = ncores)

end_time <- Sys.time()

end_time - start_time #' 20 seconds for one sample

lambda_samples_df <- bind_rows(lapply(samples, "[[", "lambda"))
prob_samples_df <- bind_rows(lapply(samples, "[[", "prob"))
prob_predictive_samples_df <- bind_rows(lapply(samples, "[[", "prob_predictive"))

#' Helper functions
row_summary <- function(df, ...) unname(apply(df, MARGIN = 1, ...))
median <- function(x) quantile(x, 0.5, na.rm = TRUE)
lower <- function(x) quantile(x, 0.025, na.rm = TRUE)
upper <- function(x) quantile(x, 0.975, na.rm = TRUE)

#' Quantile of the observation within posterior predictive
prob_predictive_quantile <- prob_predictive_samples_df %>%
  mutate(estimate = df_3$estimate_raw) %>%
  apply(MARGIN = 1, function(x) {
    estimate <- x[S + 1]
    samples <- x[1:S]
    if(all(is.na(samples))) return(NA)
    else ecdf(samples)(estimate)
  })

#' Calculate mean, median, lower and upper for each set of samples
df_3 <- df_3 %>%
  mutate(
    lambda_mean = row_summary(lambda_samples_df, mean),
    lambda_median = row_summary(lambda_samples_df, median),
    lambda_lower = row_summary(lambda_samples_df, lower),
    lambda_upper = row_summary(lambda_samples_df, upper),
    prob_mean = row_summary(prob_samples_df, mean),
    prob_median = row_summary(prob_samples_df, median),
    prob_lower = row_summary(prob_samples_df, lower),
    prob_upper = row_summary(prob_samples_df, upper),
    prob_predictive_mean = row_summary(prob_predictive_samples_df, mean),
    prob_predictive_median = row_summary(prob_predictive_samples_df, median),
    prob_predictive_lower = row_summary(prob_predictive_samples_df, lower),
    prob_predictive_upper = row_summary(prob_predictive_samples_df, upper),
    prob_predictive_quantile = prob_predictive_quantile
  )

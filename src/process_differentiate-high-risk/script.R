#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("process_differentiate-high-risk")
# setwd("src/process_differentiate-high-risk")

df_3 <- read_csv("depends/multi-sexbehav-sae.csv") %>%
  filter(model == "Model 4") #' Temporary solution, should be earlier in pipeline

df_prop <- read_csv("depends/best-fsw-logit-sae.csv")
df_prop_distinct <- distinct(df_prop, age_group, area_id, .keep_all = TRUE)

#' Add column to df_3 containing obs_idx of relevant row in df_prop_distinct for splitting sexnonregplus
df_3 <- df_3 %>%
  left_join(
    df_prop_distinct %>%
      select(age_group, area_id, prop_obs_idx = obs_idx),
    by = c("age_group", "area_id")
  )

S <- 4
full_samples <- readRDS("depends/multi-sexbehav-sae-samples.rds")

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

#' Add identifier columns
#' This assumes that the rows of eta_samples_df correspond to rows of df_3
#' It would be preferable if there were an identifier columns to be more
#' sure that this is correct, but I'm not sure it can be provided by R-INLA
eta_samples_df <- eta_samples_df  %>%
  mutate(
    #' To split by
    obs_idx = df_3$obs_idx,
    #' To sample predictive
    #' When n_eff_kish is missing there is no survey for that observation,
    #' so the posterior predictive is meaningless. Setting to zero may save
    #' some computation, but probably better to filter out entirely.
    n_eff_kish_new = floor(ifelse(is.na(df_3$n_eff_kish), 0, df_3$n_eff_kish)),
    prop_obs_idx = df_3$prop_obs_idx
  )

#' Now the logistic regression model
full_samples_prop <- readRDS("depends/fsw-logit-sae-samples.rds")
eta_samples_prop <- lapply(full_samples_prop, "[", "latent")

#' Again have to extract out a smaller part of the "latent" field
eta_samples_prop <- lapply(eta_samples_prop, function(eta_sample) {
  data.frame(eta_sample) %>%
    tibble::rownames_to_column() %>%
    rename(eta = 2) %>%
    filter(substr(rowname, 1, 10) == "Predictor:") %>%
    select(-rowname)
})

eta_samples_prop_matrix <- matrix(unlist(eta_samples_prop), ncol = S)
samples_prop_matrix <- plogis(eta_samples_prop_matrix)
samples_prop_df <- data.frame(samples_prop_matrix)

#' Useful to have this pre-split for the mclapply
samples_prop_df_split <- samples_prop_df %>%
  mutate(prop_obs_idx = df_prop$obs_idx) %>%
  filter(prop_obs_idx %in% df_prop_distinct$obs_idx) %>%
  split(.$prop_obs_idx)

stopifnot(length(samples_prop_df_split) == nrow(df_prop_distinct))

cores <- detectCores()
ncores <- 2

start_time <- Sys.time()

#' The aim here is to manipulate the samples from the three category
#' multinomial-Poisson model (stored in the dataframe eta_samples_df)
#' together with the samples from the two category logistic model
#' (stored in the dataframe samples_prop_df) to generate samples
#' of the probabilities from a four category model. These samples
#' are then summarised via the mean, median, 95% credible interval
#' ready to be added as columns to the main results dataframe.

samples <- eta_samples_df %>%
  split(.$obs_idx) %>%
  mclapply(function(x) {

    #' 1. Take softmax of the eta samples

    #' Remove the information columns
    eta_samples <- x[1:S]
    #' Normalise each column (to avoid overflow of softmax)
    eta_samples <- apply(eta_samples, MARGIN = 2, FUN = function(x) x - max(x))
    #' Exponentiate (can be done outside apply)
    #' WARNING: These aren't samples from lambda posterior! Come back to this
    lambda_samples <- exp(eta_samples)
    #' Calculate samples from posterior of probabilities
    prob_samples <- apply(lambda_samples, MARGIN = 2, FUN = function(x) x / sum(x))

    #' 2. Obtain the logisitic samples

    #' Pick out the correct sample from logistic regression model
    samples_prop <- samples_prop_df_split[[as.character(x$prop_obs_idx[1])]]
    #' Just the samples (no extra information)
    samples_prop <- samples_prop[1:S]

    #' 3. Calculate predictive samples (including sampling variability)

    n_eff_kish_new <- x[["n_eff_kish_new"]]
    prob_predictive_samples <- apply(prob_samples, MARGIN = 2, FUN = function(x) {
      stats::rmultinom(n = 1, size = n_eff_kish_new, prob = x) / n_eff_kish_new
    })

    #' 4. Differentiate the third row into the fourth and fifth rows

    #' (1) nosex12m, (2) sexcohab, (3) sexnonregplus, (4) sexnonreg, (5) sexpaid12m
    prob_samples <- data.frame(prob_samples)
    prob_samples[4, ] <- prob_samples[3, ] * (1 - samples_prop)
    prob_samples[5, ] <- prob_samples[3, ] * samples_prop

    #' 5. Return list allowing extraction of each set of samples

    list(
      lambda = data.frame(lambda_samples),
      prob = prob_samples,
      prob_predictive = data.frame(prob_predictive_samples)
    )

  }, mc.cores = ncores)

end_time <- Sys.time()

end_time - start_time

#' Includes lambda samples for nosex12m, sexcohab and sexnonregplus
lambda_samples_df <- bind_rows(lapply(samples, "[[", "lambda"))
stopifnot(nrow(df_3) == nrow(lambda_samples_df))

#' Includes prob samples for nosex12m, sexcohab, sexnonregplus, sexnonreg and sexpaid12m
prob_samples_df <- bind_rows(lapply(samples, "[[", "prob"))
stopifnot(nrow(df_3) == nrow(prob_samples_df) / 5 * 3)

#' Includes prob_predictive samples for nosex12m, sexcohab and sexnonregplus
prob_predictive_samples_df <- bind_rows(lapply(samples, "[[", "prob_predictive"))
stopifnot(nrow(df_3) == nrow(prob_predictive_samples_df))

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

#' Calculate mean, median, lower and upper for the lambda and prob_predictive samples
#' As well as adding the column for the observation quantile within posterior predictive
df_3 <- df_3 %>%
  mutate(
    lambda_mean = row_summary(lambda_samples_df, mean),
    lambda_median = row_summary(lambda_samples_df, median),
    lambda_lower = row_summary(lambda_samples_df, lower),
    lambda_upper = row_summary(lambda_samples_df, upper),
    prob_predictive_mean = row_summary(prob_predictive_samples_df, mean),
    prob_predictive_median = row_summary(prob_predictive_samples_df, median),
    prob_predictive_lower = row_summary(prob_predictive_samples_df, lower),
    prob_predictive_upper = row_summary(prob_predictive_samples_df, upper),
    prob_predictive_quantile = prob_predictive_quantile
  )

#' Add sexnonreg and sexpaid12m rows to df_3
df_sexnonregplus <- filter(df_3, indicator == "sexnonregplus")
df_sexnonreg <- replace(df_sexnonregplus, 1, "sexnonreg")
df_sexpaid12m <- replace(df_sexnonregplus, 1, "sexpaid12m")
df <- bind_rows(df_3, df_sexnonreg, df_sexpaid12m) %>%
  arrange(obs_idx)

stopifnot(nrow(df) == nrow(prob_samples_df))

#' Calculate mean, median, lower and upper for the lambda and prob_predictive samples
df <- df %>%
  mutate(
    prob_mean = row_summary(prob_samples_df, mean),
    prob_median = row_summary(prob_samples_df, median),
    prob_lower = row_summary(prob_samples_df, lower),
    prob_upper = row_summary(prob_samples_df, upper)
  )

#' Save output!
write_csv(df, "best-3p1-multi-sexbehav-sae.csv")

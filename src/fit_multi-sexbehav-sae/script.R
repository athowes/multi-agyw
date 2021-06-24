#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("fit_multi-sexbehav-sae")
# setwd("src/fit_multi-sexbehav-sae")

#' Note that this script requires a specific version of the naomi.utils package
#' This can be installed by following:
# remotes::install_github("mrc-ide/naomi.utils@sexbehav-vars")

#' 1. Get and clean the data

#' Get data
mwi <- create_surveys_dhs("MWI")
dhs <- create_individual_dhs(mwi)
sexbehav <- create_sexbehav_dhs(mwi) # Requires loading rdhs first

#' We use 4 categories, following Katie
#' Only considering AGYW (female and under 30)
data <- sexbehav %>%
  as_tibble %>%
	mutate(nosex12m = 1 - sex12m,
	       nosex = 1 - eversex) %>%
  select(-sex12m, -eversex, -sti12m) %>%
  relocate(nosex, nosex12m, .after = individual_id) %>%
	bind_cols(select(dhs, cluster_id, sex, age, indweight)) %>%
  filter(sex == "female", age < 30) %>%
  mutate(age_idx = findInterval(age, c(15, 20, 25, 30)))

#' Examine the data with NA values
na_data <- data[!complete.cases(data), ]

#' The proportion of the data that has missing values in one of the rows
nrow(na_data) / nrow(data)

#' Most of the NA entries are in sexpaid12m, with a minority in nosex
is.na(na_data) %>% colSums()

#' Which age categories have the NA entries?
#' Mostly the 15-19
data %>%
  group_by(age_idx) %>%
  summarise(
    across(
      nosex:sexpaid12m,
      list(tot = ~ sum(is.na(.x)),
           pro = ~ sum(is.na(.x)) / n())
    )
  ) %>%
  mutate_all(round, 2)

#' Tending to have NA values when nosex = 1 or nosex12m = 1 (or NA)
data %>%
  group_by(nosex, nosex12m) %>%
  summarise(
    across(
      sexcohab:sexpaid12m,
      list(tot = ~ sum(is.na(.x)),
           pro = ~ sum(is.na(.x)) / n())
    )
  ) %>%
  mutate_all(round, 2)

#' When nosex = 1, set sexpaid12m to be 0
data <- data %>%
  mutate(sexpaid12m = ifelse(nosex12m == 1, 0, sexpaid12m))

#' Remove remaining rows with NA values (complete-case analysis)
use_data <- na.omit(data)

#' Check the empirical proportions in each category
use_data %>%
  select(nosex, nosex12m, sexcohab, sexnonreg, sexpaid12m) %>%
  colMeans()

#' Check for overlapping (in more that one category) both with eversex and sex12m
complete_eversex <- use_data %>%
	mutate(r_sum = nosex + sexcohab + sexnonreg + sexpaid12m) %>%
	filter(r_sum == 1) %>%
	select(-r_sum, -nosex12m)

complete_sex12m <- use_data %>%
  mutate(r_sum = nosex12m + sexcohab + sexnonreg + sexpaid12m) %>%
  filter(r_sum == 1) %>%
  select(-r_sum, -nosex)

# This is the proportion which has r_sum == 1
nrow(complete_eversex) / nrow(use_data)
nrow(complete_sex12m) / nrow(use_data)

#' Study the incomplete rows
incomplete_eversex <- use_data %>%
  mutate(r_sum = nosex + sexcohab + sexnonreg + sexpaid12m) %>%
  filter(r_sum != 1) %>%
  select(-nosex12m)

#' Not all of these have an r_sum value of 2
summary(incomplete_eversex$r_sum)

#' Joint occurrence of
#' 0 in everything
#' sexnonreg, sexpaid12m
#' sexcohab, sexpaid12m
#' Later two easy to solve, first less so
incomplete_eversex %>%
  group_by(nosex, sexcohab, sexnonreg, sexpaid12m) %>%
  summarise(count = n())

#' What about for sex12m?
incomplete_sex12m <- use_data %>%
  mutate(r_sum = nosex12m + sexcohab + sexnonreg + sexpaid12m) %>%
  filter(r_sum != 1) %>%
  select(-nosex)

#' All of them have an r_sum value of 2
summary(incomplete_sex12m$r_sum)

#' Here we have joint occurrence of
#' sexnonreg, sexpaid12m
#' sexcohab, sexpaid12m
#' only
incomplete_sex12m %>%
  group_by(nosex12m, sexcohab, sexnonreg, sexpaid12m) %>%
  summarise(count = n())

#' Fix data by setting all rows with have sexpaid12m to be in that category
#' Use sex12m, as I think this fits the data best as a multinomial category
use_data <- use_data %>%
  mutate(nosex = ifelse(sexpaid12m == 1, 0, nosex),
         nosex12m = ifelse(sexpaid12m == 1, 0, nosex12m),
         sexcohab = ifelse(sexpaid12m == 1, 0, sexcohab),
         sexnonreg = ifelse(sexpaid12m == 1, 0, sexnonreg)) %>%
  select(-nosex)

#' Verify no overlap
stopifnot(
  mutate(use_data, r_sum = nosex12m + sexcohab + sexnonreg + sexpaid12m) %>%
    filter(r_sum != 1) %>% nrow() == 0
)

#' 2. Fit individual level model

#' This considers each individual as a single multinomial trial
#' It may be easier to get the weight into the likelihood but would take longer time to fit
data_indiv <- use_data %>%
	select(-survey_id, -individual_id, -indweight, -sex, -age) %>%
	mutate(obs_idx = 1:n()) %>%
	pivot_longer(nosex12m:sexpaid12m, values_to = "y", names_to = "cat_idx") %>%
	mutate(cat_idx = as.integer(as.factor(cat_idx)),
	       age_cat_idx = interaction(age_idx, cat_idx))

#' Try fitting a model using the individual level data
tau_prior <- function(x) list(prec = list(initial = log(x), fixed = TRUE))

formula <- y ~ -1 + f(obs_idx, hyper = tau_prior(0.000001)) +
  f(cat_idx, model = "iid", constr = TRUE, hyper = tau_prior(0.001)) +
  f(age_cat_idx, model = "iid", constr = TRUE, hyper = tau_prior(0.001))

fit_indiv <- inla(formula, data = data_indiv, family = "Poisson",
                  control.predictor = list(link = 1),
                  control.compute = list(config = TRUE))

data_indiv$eta <- fit_indiv$summary.linear.predictor$mean

#' Compute probabilities
prob <- zoo::rollapply(data_indiv$eta, 4, by = 4, function(x) exp(x) / sum(exp(x)), partial = TRUE, align = "left")
data_indiv$prob <- as.vector(t(prob))

pred_indiv <- data_indiv %>%
  group_by(age_idx, cat_idx) %>%
  summarise(prob = mean(prob)) %>%
  spread(key = cat_idx, value = prob) %>%
  rename(prob_nosex = `1`,
         prob_sexcohab = `2`,
         prob_sexnonreg = `3`,
         prob_sexpaid12m = `4`)

#' These are the empirical probabilities from the data for each of the three age categories
truth <- use_data %>%
  group_by(age_idx) %>%
  summarise(prob_nosex12m = mean(nosex12m),
            prob_sexcohab = mean(sexcohab),
            prob_sexnonreg = mean(sexnonreg),
            prob_sexpaid12m = mean(sexpaid12m))

#' There is some variation from the emprical probabilities
list(truth = truth, pred = pred_indiv)

#' 3. Fit aggregate level model

#' This considers each combination of age and cluster_id as a multinomial trial of size n_i
#' TODO: How to treat weights, n_eff_kish?
#' Note that the weight corresponds to the individual, not to any feature of the question
#' TODO: Map cluster_id to regions of interest
data_aggregate <- use_data %>%
  select(-survey_id, -individual_id, -indweight, -sex, -age) %>%
	group_by(cluster_id, age_idx) %>%
	summarise(nosex12m = sum(nosex12m),
						sexcohab = sum(sexcohab),
						sexnonreg = sum(sexnonreg),
						sexpaid12m = sum(sexpaid12m)) %>%
	ungroup() %>%
  mutate(m = nosex12m + sexcohab + sexnonreg + sexpaid12m,
         obs_idx = 1:n()) %>%
	pivot_longer(nosex12m:sexpaid12m, values_to = "y", names_to = "cat_idx") %>%
  mutate(cat_idx = as.integer(as.factor(cat_idx)),
         age_cat_idx = interaction(age_idx, cat_idx))

#' Model for the aggregated data, using the Poisson trick
#' TODO: Does the sample size n_i play into this at all?_
formula <- y ~ -1 + f(obs_idx, hyper = tau_prior(0.000001)) +
  f(age_cat_idx, model = "iid", constr = TRUE, hyper = tau_prior(0.001))
  #' TODO: Make graph for cluster_id
  #' f(cluster_id, model = "besag", graph = G, constr = T)

fit_aggregate <- inla(formula, data = data_aggregate, family = 'Poisson',
					            control.predictor = list(link = 1), control.compute = list(config = TRUE))

data_aggregate$eta <- fit_aggregate$summary.linear.predictor$mean

#' Compute probabilities
prob <- zoo::rollapply(data_aggregate$eta, 4, by = 4, function(x) exp(x) / sum(exp(x)), partial = TRUE, align = "left")
data_aggregate$prob <- as.vector(t(prob))

pred_aggregate <- data_aggregate %>%
  group_by(age_idx, cat_idx) %>%
  summarise(prob = mean(prob)) %>%
  spread(key = cat_idx, value = prob) %>%
  rename(prob_nosex = `1`,
         prob_sexcohab = `2`,
         prob_sexnonreg = `3`,
         prob_sexpaid12m = `4`)

#' Close to exactly recovering the empirical probabilities
list(truth = truth, pred = pred_aggregate)

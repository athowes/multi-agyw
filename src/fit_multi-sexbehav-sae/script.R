#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("fit_multi-sexbehav-sae")
# setwd("src/fit_multi-sexbehav-sae")

#' Note that this script requires a specific version of the naomi.utils package
#' This can be installed by following:
# remotes::install_github("mrc-ide/naomi.utils@sexbehav-vars")

#' Get data
mwi <- create_surveys_dhs("MWI")
dhs <- create_individual_dhs(mwi)
sexbehav <- create_sexbehav_dhs(mwi) # Requires loading rdhs first

#' We use 4 categories, following Katie
use_data <- sexbehav %>%
  as_tibble %>%
	mutate(nosex12m = 1 - sex12m,
	       nosex = 1 - eversex) %>%
  select(-sex12m, -eversex, -sti12m) %>%
  relocate(nosex, nosex12m, .after = individual_id) %>%
	bind_cols(select(dhs, cluster_id, sex, age, indweight))

#' Check the empirical proportions in each
use_data %>%
  select(nosex, nosex12m, sexcohab, sexnonreg, sexpaid12m) %>%
  colMeans(na.rm = TRUE)

#' Check for overlapping (in more that one category)

#' First do this with eversex
complete_eversex <- use_data %>%
	mutate(r_sum = nosex + sexcohab + sexnonreg + sexpaid12m) %>%
	filter(r_sum == 1) %>%
	select(-r_sum, -nosex12m)

# This is the proportion which doesn't have r_sum == 1
nrow(complete_eversex) / nrow(use_data)

#' And now with sex12m
complete_sex12m <- use_data %>%
  mutate(r_sum = nosex12m + sexcohab + sexnonreg + sexpaid12m) %>%
  filter(r_sum == 1) %>%
  select(-r_sum, -nosex)

nrow(complete_sex12m) / nrow(use_data)

#' Study the incomplete rows
incomplete_eversex <- use_data %>%
  mutate(r_sum = nosex + sexcohab + sexnonreg + sexpaid12m) %>%
  filter(r_sum != 1) %>%
  select(-nosex12m)

#' All of them have an r_sum value of 2
summary(incomplete_eversex$r_sum)

#' There is only joint occurrence of
#' sexnonreg, sexpaid12m
#' sexcohab, sexpaid12m
#' which thankfully seems quite easy to solve
incomplete_eversex %>%
  group_by(nosex, sexcohab, sexnonreg, sexpaid12m) %>%
  summarise(count = n())

#' What about for sex12m?
incomplete_sex12m <- use_data %>%
  mutate(r_sum = nosex12m + sexcohab + sexnonreg + sexpaid12m) %>%
  filter(r_sum != 1) %>%
  select(-nosex)

#' All of them also have an r_sum value of 2
summary(incomplete_sex12m$r_sum)

#' Here we have joint occurrence of
#' sexnonreg, sexpaid12m
#' sexcohab, sexpaid12m
#' nosex12m, sexpaid12m
#' which is a little more difficult (nosex12m and sexpaid12m!)
#' but still quite simple
incomplete_sex12m %>%
  group_by(nosex12m, sexcohab, sexnonreg, sexpaid12m) %>%
  summarise(count = n())

#' Work with the complete_eversex for now
complete <- complete_eversex

#' This considers each individual as a single multinomial trial
#' It may be easier to get the weight into the likelihood but would take longer time to fit
data_indiv <- complete %>%
	filter(sex == "female", age < 30) %>%
	mutate(age_idx = findInterval(age, c(15, 20, 25, 30))) %>%
	select(-survey_id, -individual_id, -indweight, -sex, -age) %>%
	mutate(obs_idx = 1:n()) %>%
	pivot_longer(nosex:sexpaid12m, values_to = "y", names_to = "cat_idx") %>%
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

pred <- data_indiv %>%
  group_by(age_idx, cat_idx) %>%
  summarise(prob = mean(prob)) %>%
  spread(key = cat_idx, value = prob) %>%
  rename(prob_nosex = `1`,
         prob_sexcohab = `2`,
         prob_sexnonreg = `3`,
         prob_sexpaid12m = `4`)

#' These are the empirical probabilities from the data for each of the three age categories
truth <- complete %>%
  filter(sex == "female", age < 30) %>%
  mutate(age_idx = findInterval(age, c(15, 20, 25, 30))) %>%
  group_by(age_idx) %>%
  summarise(prob_nosex = mean(nosex),
            prob_sexcohab = mean(sexcohab),
            prob_sexnonreg = mean(sexnonreg),
            prob_sexpaid12m = mean(sexpaid12m))

list(truth = truth, pred = pred)

#' This considers each combination of age and cluster_id as a multinomial trial of n_i
#' TODO: How to treat weights, n_eff_kish?
#' TODO: Map cluster_id to regions of interest
data_aggregate <- complete %>%
  filter(sex == 'female', age < 30) %>%
	mutate(age_idx = findInterval(age, c(15, 20, 25, 30))) %>%
  select(-survey_id, -individual_id, -indweight, -sex, -age) %>%
	group_by(cluster_id, age_idx) %>%
	summarise(nosex = sum(nosex),
						sexcohab = sum(sexcohab),
						sexnonreg = sum(sexnonreg),
						sexpaid12m = sum(sexpaid12m)) %>%
	ungroup() %>%
  mutate(obs_idx = 1:n()) %>%
	pivot_longer(nosex:sexpaid12m, values_to = "y", names_to = "cat_idx") %>%
  mutate(cat_idx = as.integer(as.factor(cat_idx)),
         age_cat_idx = interaction(age_idx, cat_idx))

#' #' Test a model, using the Poisson trick
#' formula <- y ~ -1 +
#' 	f(obs_idx, fixed = TRUE) +
#' 	f(cat_idx, age_idx, fixed = TRUE, constr = TRUE)
#' 	#' f(cluster_id, model = "besag", graph = G, constr = T)
#' 	#' TODO: make graph for id cluster
#'
#' fit_aggregate <- inla(formula, data = data_aggregate, family = 'Poisson',
#' 					            control.predictor = list(link = 1), control.compute = list(config = TRUE))
#'
#' #' TODO: get prediction of prob.
#' fit_aggregate$summary.fitted %>% str
#' fit_aggregate$summary.random$cat_idx$mean
#' fit_aggregate$summary.random$obs_idx$mean %>% plot

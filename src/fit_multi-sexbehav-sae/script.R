#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("data_prep")
# setwd("src/data_prep")

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
	bind_cols(select(dhs, cluster_id, sex, age, indweight))

#' Check for overlapping (in more that one category) and ignore them for now
#' TODO: Find a better way to deal with this

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
	filter(sex == "female", age <= 30) %>%
	mutate(age_idx = findInterval(age, c(15, 20, 25, 30))) %>%
	select(-survey_id, -individual_id, -indweight, -sex, -age) %>%
	mutate(obs_idx = 1:n()) %>%
	pivot_longer(nosex:sexpaid12m, values_to = "y", names_to = "cat_idx") %>%
	mutate(cat_idx = as.integer(as.factor(cat_idx)))

#' This considers each combination of age and cluster_id as a multinomial trial of n_i
#' TODO: How to treat weights, n_eff_kish?
#' TODO: Map cluster_id to regions of interests
data_aggregate <- complete %>%
  filter(sex == 'female', age <= 30) %>%
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
	mutate(cat_idx = as.integer(as.factor(cat_idx)))

#' Test a model, using the Poisson trick
formula <- y ~ -1 +
	f(obs_idx, fixed = TRUE) +
	f(cat_idx, age_idx, fixed = TRUE, constr = TRUE)
	#' f(cluster_id, model = "besag", graph = G, constr = T)
	#' TODO: make graph for id cluster

fit <- inla(formula, data = data_aggregate, family = 'Poisson',
					 control.predictor = list(link = 1), control.compute = list(config = TRUE))

#' TODO: get prediction of prob.
fit$summary.fitted %>% str
fit$summary.random$cat_idx$mean
fit$summary.random$obs_idx$mean %>% plot

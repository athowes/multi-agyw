# remotes::install_github("mrc-ide/naomi.utils@sexbehav-vars")
library(naomi.utils)
library(rdhs)
library(tidyr)
library(dplyr)

# get data
mwi <- create_surveys_dhs("MWI")
dhs <- create_individual_dhs(mwi)
sexbehav <- create_sexbehav_dhs(mwi)   # load rdhs first

# we use 4 cat. as Katie
usedata <- 
	sexbehav %>% as_tibble %>% 
	select(survey_id, individual_id, eversex, sexcohab, sexnonreg, sexpaid12m) %>% 
	mutate(eversex = 1-eversex) %>% rename(nosex = eversex) %>% 
	bind_cols(select(dhs, cluster_id, sex, age, indweight))

# check for overlapping and ignore theme
# TODO: how to treat the overlapping
good_ones <- 
	usedata %>% 
	mutate(r_sum = nosex + sexcohab + sexnonreg + sexpaid12m) %>% 
	filter(r_sum == 1) %>% 
	select(-r_sum)

# This considers each individial a multinomial trial prob. easier to get the
# weight into the likelihood but would take longer time to fit
fit_data_indiv <- 
	good_ones %>% 
	filter(sex=='female') %>%
	mutate(agegrp = findInterval(age, c(15, 20, 25, 30))) %>%
	select(-char(survey_id, individual_id, indweight, sex, age)) %>% 
	mutate(id = 1:n()) %>% 
	pivot_longer(nosex:sexpaid12m, values_to = "Y", names_to = "cat_id") %>% 
	mutate(cat_id = as.integer(as.factor(cat_id)))

# This considers each combination of age and cluster_id a multinomial trial of n_i
# TODO: how to treat weights, n_eff_kish?
# TODO: map cluster_id to regional of interests
fit_data_aggregate <- 
	good_ones %>% 
	filter(sex=='female') %>%
	mutate(agegrp = findInterval(age, c(15, 20, 25, 30))) %>%
	select(-char(survey_id, individual_id, indweight, sex, age)) %>% 
	group_by(cluster_id, agegrp) %>% 
	summarise(nosex = sum(nosex), 
						sexcohab = sum(sexcohab), 
						sexnonreg = sum(sexnonreg), 
						sexpaid12m = sum(sexpaid12m)) %>% 
	ungroup() %>% mutate(id = 1:n())  %>% 
	pivot_longer(nosex:sexpaid12m, values_to = "Y", names_to = "cat_id") %>% 
	mutate(cat_id = as.integer(as.factor(cat_id))) 

library(INLA)

# Test a model-Poisson trick
fm = Y ~ -1 + 
	f(id, fixed = T) +
	f(cat_id, agegrp, fixed = T, constr = T) 
	# f(cluster_id, model = "besag", graph = G, constr = T)
	# TODO: make graph for id cluster
	
m1 = inla(fm, data = fit_data_aggregate, family = 'Poisson', 
					control.predictor = list(link = 1), control.compute = list(config = TRUE))

# TODO: get prediction of prob. 
m1$summary.fitted %>% str
m1$summary.random$cat_id$mean 
m1$summary.random$id$mean %>% plot


#' This is how I learnt how to do the MP transformation
#' Perhaps could make a good blog post, tutorial etc. (i.e. showing why some of the things I tried are wrong)

#' Start by simulating some data

analysis_level <- c("MWI" = 5)
admin1_level <- c("MWI" = 1)

stopifnot(iso3 %in% names(analysis_level))
stopifnot(iso3 %in% names(admin1_level))

analysis_level <- analysis_level[iso3]
admin1_level <- admin1_level[iso3]

#' Going to need the Malawi area files here
areas <- read_sf(paste0("depends/", tolower(iso3), "_areas.geojson"))

areas <- select(areas, area_id, area_name, area_level, area_level_label,
                parent_area_id, area_sort_order, center_x, center_y)

areas_model <- filter(areas, area_level == analysis_level)

areas_model <- areas_model %>%
  left_join(
    areas %>%
      st_drop_geometry() %>%
      filter(area_level <= analysis_level) %>%
      spread_areas() %>%
      select(area_id, area_id_aggr = paste0("area_id", admin1_level)),
    by = "area_id"
  )

#' Add an integer index for INLA
areas_model <- areas_model %>%
  arrange(area_sort_order) %>%
  mutate(area_idx = row_number())

#' Only using three age groups here
age_groups <- c("Y015_019", "Y020_024", "Y025_029")

df <- crossing(
  age_group = age_groups,
  areas_model %>%
    st_drop_geometry() %>%
    select(area_id, area_name, area_idx, area_id_aggr,
           area_sort_order, center_x, center_y)
)

#' Assume these four categories to be exhaustive and mutually exclusive
indicators <- c("nosex12m", "sexcohab", "sexnonreg", "sexpaid12m")
K <- length(indicators)

#' Make up value of the multinomial probabilities for simulation
#' No spatial structure for now, same probabilities for all areas
prob_Y015_019 <- c(0.6, 0.25, 0.12, 0.03)
prob_Y020_024 <- c(0.2, 0.4, 0.35, 0.05)
prob_Y025_029 <- c(0.05, 0.5, 0.4, 0.05)

#` Sample size for each area age combination is equal
m <- 50

#' Copy df K times, adding cat_idx, representing the category number (1, ..., K)
#' Then copy this another m times, representing the m multinomial observations
df <- df %>%
  dplyr::slice(rep(1:n(), each = K)) %>%
  dplyr::mutate(cat_idx = rep(1:K, times = n() / K)) %>%
  dplyr::slice(rep(row_number(), m))

#' Tall format: (1 x n * K) length vector
tall_rmultinomial <- function(n, prob) {
  replicate(n, rmultinom(1, 1, prob = prob), simplify = TRUE) %>%
    as.vector()
}

#' nrow(areas_model) is the number of areas
samp_Y015_019 <- tall_rmultinomial(n = m * nrow(areas_model), prob = prob_Y015_019)
samp_Y020_024 <- tall_rmultinomial(n = m * nrow(areas_model), prob = prob_Y020_024)
samp_Y025_029 <- tall_rmultinomial(n = m * nrow(areas_model), prob = prob_Y025_029)

#' Add multinomial samples data, and index for the observation to df
#' There are a total of m * nrow(areas_model) multinomial observations for each age category
#' and the total number of rows of df is given as follows
stopifnot(nrow(df) == nrow(areas_model) * m * length(age_groups) * K)

df <- df %>%
  arrange(age_group) %>%
  mutate(obs_idx = rep(1:(m * nrow(areas_model) * length(age_groups)), each = K),
         y = c(samp_Y015_019, samp_Y020_024, samp_Y025_029))

#' See Serafini's "Multinomial logit models with INLA" tutorial
#' https://inla.r-inla-download.org/r-inla.org/doc/vignettes/multinomial.pdf

#' Model 1.
#' No intercept (-1) and observation specific intercepts f(obs_idx, ...)
formula1 <- y ~ -1 + f(obs_idx, hyper = list(prec = list(initial = log(0.000001), fixed = TRUE)))

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
formula2 <- y ~ -1 + f(obs_idx, hyper = list(prec = list(initial = log(0.000001), fixed = TRUE))) +
  f(cat_idx, model = "iid", constr = TRUE, hyper = list(prec = list(initial = log(0.001), fixed = TRUE)))

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
colnames(prob) <- c("nosex12m", "sexcohab", "sexnonreg", "sexpaid12m")
cbind(truth = colMeans(prob), pred = exp(etas) / sum(exp(etas)))

#' Model 3.
#' Add age specific random effects

df <- mutate(df, age_idx = case_when(
  age_group == "Y015_019" ~ 1,
  age_group == "Y020_024" ~ 2,
  age_group == "Y025_029" ~ 3)
)

formula3 <- y ~ -1 + f(obs_idx, hyper = list(prec = list(initial = log(0.000001), fixed = TRUE))) +
  f(cat_idx, model = "iid", constr = TRUE, hyper = list(prec = list(initial = log(0.001), fixed = TRUE))) +
  f(age_idx, model = "iid", constr = TRUE, hyper = list(prec = list(initial = log(0.001), fixed = TRUE)))

fit3 <- inla(formula3, data = df, family = "Poisson",
             control.predictor = list(link = 1),
             control.compute = list(config = TRUE))

#' The first indexes for 15-19 are 1:4, 20-24 are 6601:6604 and 25-29 are 13201:13204
i_start_Y015_019 <- min(which(df$age_idx == 1))
i_start_Y020_024 <- min(which(df$age_idx == 2))
i_start_Y025_029 <- min(which(df$age_idx == 3))

#' Softmax function https://en.wikipedia.org/wiki/Softmax_function
softmax <- function(x) exp(x) / sum(exp(x))

calculate_pred <- function(fit) {
  etas_Y015_019 <- fit$summary.linear.predictor$mean[i_start_Y015_019:(i_start_Y015_019 + 3)]
  etas_Y020_024 <- fit$summary.linear.predictor$mean[i_start_Y020_024:(i_start_Y020_024 + 3)]
  etas_Y025_029 <- fit$summary.linear.predictor$mean[i_start_Y025_029:(i_start_Y025_029 + 3)]

  pred <- t(sapply(list(etas_Y015_019, etas_Y020_024, etas_Y025_029), softmax))

  rownames(pred) <- c("Y015_019", "Y020_024", "Y025_029")
  colnames(pred) <- c("nosex12m", "sexcohab", "sexnonreg", "sexpaid12m")

  return(pred)
}

#' Getting this wrong still!
list(truth = prob, pred = calculate_pred(fit3))

#' The category specific random effects are the same as in Model 2
fit3$summary.random$cat_idx
fit2$summary.random$cat_idx

#' The age specific random effects are approximately zero
fit3$summary.random$age_idx

#' The empirical probabilities show it's not a problem with the simulated data
df %>%
  group_by(age_group, cat_idx) %>%
  summarise(y_mean = mean(y))

#' The problem is that
#' exp(eta + c) / sum(exp(eta + c)) = exp(eta) / exp
#' For example
exp(etas) / sum(exp(etas))
exp(etas + 1) / sum(exp(etas + 1))
#' So there is no use in having an age effect without the interaction

#' Model 4.
#' Age x category interactions rather than age random effects

df <- mutate(df, age_cat_idx = interaction(age_idx, cat_idx))

formula4 <- y ~ -1 + f(obs_idx, hyper = list(prec = list(initial = log(0.000001), fixed = TRUE))) +
  f(cat_idx, model = "iid", constr = TRUE, hyper = list(prec = list(initial = log(0.001), fixed = TRUE))) +
  f(age_cat_idx, model = "iid", constr = TRUE, hyper = list(prec = list(initial = log(0.001), fixed = TRUE)))

fit4 <- inla(formula4, data = df, family = "Poisson",
             control.predictor = list(link = 1),
             control.compute = list(config = TRUE))

#' Got it!
list(truth = prob, pred = calculate_pred(fit4))

#' Take S samples from the full posterior
S <- 1000
full_samples <- inla.posterior.sample(n = S, result = fit4)
prob_samples <- lapply(full_samples, function(x) {
  df <- t(data.frame(
    Y015_019 = softmax(x$latent[i_start_Y015_019:(i_start_Y015_019 + 3)]),
    Y020_024 = softmax(x$latent[i_start_Y020_024:(i_start_Y020_024 + 3)]),
    Y025_029 = softmax(x$latent[i_start_Y025_029:(i_start_Y025_029 + 3)])
  ))
  colnames(df) <- c("Not sexually active", "Cohabiting partner", "Non-regular partner(s)", "Key populations")
  rownames(df) <- c("15-19", "20-24", "25-29")
  return(df)
})

list_to_df <- function(list){
  data.frame(dplyr::bind_rows(list, .id = "replicate"))
}

tall_df <- lapply(prob_samples, function(x) as.data.frame(as.table(x))) %>%
  list_to_df() %>%
  rename(age_group = Var1,
         category = Var2,
         value = Freq)

#' Plots to represent the category probabilities and uncertainty

#' Barplot with standard error
tall_df %>%
  group_by(age_group, category) %>%
  summarise(est = mean(value),
            se = sd(value) / sqrt(n())) %>%
  ggplot(aes(x = category, y = est, fill = category)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  geom_errorbar(aes(ymin = est - 2 * se, ymax = est + 2 * se),
                width  = 0.2, position = position_dodge(0.9)) +
  facet_wrap(~age_group) +
  viridis::scale_fill_viridis(discrete = TRUE) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Proportion", fill = "Risk group") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#' Stacked barplot
tall_df %>%
  group_by(age_group, category) %>%
  summarise(est = mean(value),
            se = sd(value) / sqrt(n())) %>%
  ggplot(aes(x = age_group, y = est, fill = forcats::fct_rev(category))) +
  geom_bar(stat = "identity", position = "fill", alpha = 0.8) +
  viridis::scale_fill_viridis(discrete = TRUE, direction = -1) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Age", y = "Proportion", fill = "Risk group")

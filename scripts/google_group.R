# From https://groups.google.com/g/r-inla-discussion-group/c/CB7SnisEpPI/m/TkHG9p4_CAAJ

library(INLA)
library(tidyverse)

# Number of groups / categories
ng <- 3

# Number of replications
n <- 1000

# Sample size for each multinomial observation
size <- 20

y <- matrix(NA, n, ng) # Data
z <- matrix(NA, n, ng) # A {0, 1} covariate
p <- matrix(NA, n, ng) # Probabilities
g <- matrix(NA, n, ng) # Group identity

for(i in 1:n) {
  g[i,] <- i
  z[i,] <- sample(0:1, size = ng, replace = TRUE)
  p[i,] <- exp(z[i, ]) / sum(exp(z[i, ]))
  y[i,] <- rmultinom(n = 1, size = size, prob = p[i, ])
}

# As vectors rather than matrices for R-INLA
# Using t() as I want data from the same multinomial observation together
y <- as.vector(t(y))
g <- as.vector(t(g)) # Equivalent to rep(1:n, each = ng)
z <- as.vector(t(z))

# The formulation model = "iid", initial = -5, fixed = TRUE
# is used to model the observation specific intercept
formula <- y ~ z + f(g, model = "iid", hyper = list(prec = list(initial = -5, fixed = T))) - 1

r <- inla(
  formula,
  data = data.frame(y, z, g),
  family = "poisson",
  control.compute = list(config = TRUE),
  control.predictor = list(compute = TRUE),
  verbose = FALSE
)

summary(r)

# Take 1000 samples from the full posterior
nsample <- 1000
xx <- inla.posterior.sample(nsample, r)

# Just predicting the first multinomial observation probabilities
target <- paste0("Predictor:", 1:3)
prob1 <- matrix(NA, nsample, 3)

for(i in 1:nsample) {
  eta <- xx[[i]]$latent[target, 1]
  prob1[i, ] <- exp(eta) / sum(exp(eta))
}

prob1 %>%
  as.data.frame() %>%
  reshape2::melt() %>%
  ggplot(aes(x = value, fill = variable)) +
    geom_density(alpha = 0.3) +
    labs(x = "Probability", y = "Posterior density")

prob1_mean <- colMeans(prob)

print(
 rbind(predicted = prob1_mean, truth = p[1:3])
)

# Alternative way, using $summary.linear.predictor
linear_predictor_mean <- r$summary.linear.predictor$mean
denom <- tapply(linear_predictor_mean, g, FUN = function(x) sum(exp(x)))
prob_mean <- exp(linear_predictor_mean) / rep(denom, each = 3)

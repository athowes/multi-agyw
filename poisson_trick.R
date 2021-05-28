# https://inla.r-inla-download.org/r-inla.org/doc/vignettes/multinomial.pdf

set.seed(1)
library(INLA)

# Number of observations
n <- 10

# Covariates
x <- rnorm(n, sd = 10)

# Linear predictor
eta <- 1 + x

# Probabilities
prob <- plogis(eta)

# Sample sizes
n_trials <- sample(100:1000, n, replace = TRUE)

# Observations
y <- rbinom(n, size = n_trials, prob = prob)

lprec <- 25

# The standard INLA model using family = "binomial"
fit <- inla(
  y ~ -1 + intercept + x,
  data = list(y = y, x = x, Ntrials = n_trials, intercept = rep(1, n)),
  family = "binomial",
  Ntrials = n_trials,
  control.predictor = list(hyper = list(prec = list(initial = lprec))),
  control.inla = list(tolerance = 1E-12)
)

# Examine the results
summary(fit)

# Now using the Poisson trick
# n binomial observations become 2 * n Poisson observations

# Y = (y_1, m_1 - y_1, ..., y_n, y_n - m_n)
Y <- c(rbind(y, n_trials - y))

# xx = (x_1, 0, ..., x_n, 0)
xx <- c(rbind(x, rep(0, n)))

# intercept = (1, 0, ..., 1, 0)
intercept <- c(rbind(rep(1, n), rep(0, n)))

# Create random effects indicator
phi <- rep(1:n, each = 2)

formula <- Y ~ -1 +
  intercept +
  f(phi, model = "iid", hyper = list(prec = list(initial = -lprec, fixed = TRUE))) +
  xx

fit_pois <- inla(
  formula,
  family = "poisson",
  data = list(Y = Y, xx = xx, phi = phi, intercept = intercept),
  control.predictor = list(hyper = list(prec = list(initial = lprec))),
  control.inla = list(tolerance = 1E-12)
)

summary(fit_pois)

# The intercept marginals
plot(fit$marginals.fixed$intercept, type = "l", col = "red")
lines(fit_pois$marginals.fixed$intercept, type = "l", col = "green")

# The x covariate marginals
plot(fit$marginals.fixed$x, type = "l", col = "red")
lines(fit_pois$marginals.fixed$xx, type = "l", col = "green")

# The marginal likelihood
print(fit$mlik)

# Compute a correction factor for the Poisson model
phi_mode <- fit_pois$summary.random$phi$mode
corr_factor <- -sum(dnorm(phi_mode, sd = sqrt(1/exp(-lprec)), log = TRUE)) + sum(log(n_trials))

print(fit_pois$mlik + corr_factor)

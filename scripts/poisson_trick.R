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

# Practical part

library(Ecdat)
library(deldir)
library(sp)
library(rgeos)
library(mvtnorm)
library(gridExtra)
library(mlogit)

# Three types of variable:
# 1. Alternavitive specific variable with generic coefficient
# 2. Alternative specific variable with alternative specific coefficient
# 3. Individual specific variable with alternative specific coefficient

beta <- -0.3
deltas <- c(1, 4, 3)
gammas <- c(0.3, 0.2, 0.4)
param <- c(beta, deltas, gammas)
n <- 500

set.seed(123)

X.A <- rnorm(n, mean = 30, sd = 2.5)
X.B <- rnorm(n, mean = 35, sd = 3.5)
X.C <- rnorm(n, mean = 40, sd = 1)

W.A <- abs(rnorm(n, mean = 1))
W.B <- abs(rnorm(n, mean = 1))
W.C <- abs(rnorm(n, mean = 1))

Z <- rnorm(n, 20, 3)

multinom_sample <- function(N) {
  Y <- matrix(NA, ncol = 3, nrow = n)
  for(i in 1:n){
    V.A <- beta * X.A[i] + deltas[1] * W.A[i] + gammas[1] * Z[i]
    V.B <- beta * X.B[i] + deltas[2] * W.B[i] + gammas[2] * Z[i]
    V.C <- beta * X.C[i] + deltas[3] * W.C[i] + gammas[3] * Z[i]
    probs <- c(V.A, V.B, V.C)
    probs <- exp(probs)/sum(exp(probs))
    samp <- rmultinom(1, N, prob = probs)
    Y[i,] <- as.vector(samp)
  }
  colnames(Y ) <- c("Y.A", "Y.B", "Y.C")
  return(Y)
}

head(multinom_sample(1), 5)
head(multinom_sample(100), 5)

# The structure of the covariates depends on the type
# 1. Long format
# 2. Mixed structure
# 3. Long format

Y <- multinom_sample(1)
df <- data.frame(cbind(Y, X.A, X.B, X.C, W.A, W.B, W.C, Z))

data_structure <- function(df) {
  data <- matrix(NA, ncol = 8, nrow = n * 3)
  for(i in 1:n){
    # The ith multinomial observation
    indices <- ((3 * (i - 1)) + 1):(3 * i)

    # Simulated variable
    data[indices, 1] <- c(df$Y.A[i], df$Y.B[i], df$Y.C[i])

    # Alternative specific with generic coefficient
    data[indices, 2] <- c(df$X.A[i], df$X.B[i], df$X.C[i])

    # Alternative specific with alternative coefficient
    data[indices, 3:5] <- diag(c(df$W.A[i], df$W.B[i], df$W.C[i]))

    # Individual specific with alternative coefficient
    data[indices, 6] <- rep(df$Z[i], 3)

    # Choice situation index
    data[indices, 7] <- rep(i, 3)

    # Choice alternative index
    data[indices, 8] <- c(1, 2, 3)
  }
  data <- data.frame(data)
  names(data) <- c("Y", "X", "W.A", "W.B", "W.C", "Z", "phi", "alt.idx")
  return(data)
}

data_structure(df)

head(data_structure(df))

formula <- Y ~ -1 + X + W.A + W.B + W.C +
  f(phi, initial = -10, fixed = TRUE) +
  f(alt.idx, Z, fixed = TRUE, constr = TRUE)

r <- inla(formula, data = data_structure(df), family = "Poisson")

result <- rbind(r$summary.fixed[1:5], r$summary.random$alt.idx[2:6])
result <- cbind(result, true = param)
row.names(result) <- c("beta", "delta.A", "delta.B", "delta.C", "gamma.A", "gamma.B", "gamma.C")
round(result, 3)

diff_result <- cbind(
  "0.025quant" = diff(r$summary.random$alt.idx$`0.025quant`),
  "0.5quant" = diff(r$summary.random$alt.idx$`0.5quant`),
  "0.975quant" = diff(r$summary.random$alt.idx$`0.975quant`),
  "true" = diff(gammas)
)

row.names(diff_result) <- c("gamma.B - gamma.A", "gamma.C - gamma.B")
round(diff_result, 3) # Can retrieve the differences

# IID random effects
random_effect <- rnorm(n)

multinom_sample_rand <- function(N, random_effect) {
  Y <- matrix(NA, ncol = 3, nrow = n)
  for(i in 1:n){
    V.A <- beta * X.A[i] + deltas[1] * W.A[i] + gammas[1] * Z[i] +
      random_effect[i]
    V.B <- beta * X.B[i] + deltas[2] * W.B[i] + gammas[2] * Z[i]
    V.C <- beta * X.C[i] + deltas[3] * W.C[i] + gammas[3] * Z[i]
    probs <- c(V.A, V.B, V.C)
    probs <- exp(probs)/sum(exp(probs))
    samp <- rmultinom(1, N, prob = probs)
    Y[i,] <- as.vector(samp)
  }
  colnames(Y ) <- c("Y.A", "Y.B", "Y.C")
  return(Y)
}

Y.rand1 <- multinom_sample_rand(100, random_effect)
df.rand1 <- data.frame(cbind(Y.rand1, X.A, X.B, X.C, W.A, W.B, W.C, Z))
data.rand1 <- data_structure(df.rand1)

# The index for INLA's f()
rand.idx <- rep(NA, n * 3)
rand.idx[seq(1, n * 3, by = 3)] <- 1:n
data.rand1$rand.idx <- rand.idx
round(head(data.rand1), 3) # 1:n but only for category A, NA otherwise

formula.rand1 <- Y ~ -1 + X + W.A + W.B + W.C +
  f(phi, initial = -10, fixed = TRUE) +
  f(alt.idx, Z, fixed = TRUE, constr = TRUE) +
  f(rand.idx, model = "iid") # The random effects!

r.rand1 <- inla(formula.rand1, data = data.rand1, family = "Poisson")
result.rand1 <- rbind(r.rand1$summary.fixed[1:5])
result.rand1 <- cbind(result.rand1, true = param[1:4])
row.names(result.rand1) <- c("beta", "delta.A", "delta.B", "delta.C")
round(result.rand1, 3)

# Mean square error of random effect recovery
mean((random_effect - r.rand1$summary.random$rand.idx$mean)^2)

#' Let's try following the "Example i.i.d. model" and "Neonatal mortality" vignettes

library(makemyprior)

#' https://cran.r-project.org/web/packages/makemyprior/vignettes/make_prior.html

#' Say we have y_ij = a_i + e_ij
#' w_{a / (a + e)} is the variance proportion (just call this w)
#' sigma_star ~ PC_0(U, alpha)
#' w ~ PC_0(m) with P(w > m) = 0.5 and shrink towards 0
#' w ~ PC_1(m) with P(w > m) = 0.5 and shrink towards 1
#' w ~ PC_M(m, c) with P(w > m) = 0.5 and P(logit(0.25) < logit(w) - logit(m) < logit(0.75)) = c
#' PC_1(m) on w_{a / (a + e)} is equivalent to PC_0(m) on w_{e / (a + e)}

formula <- y ~ x + mc(a) + mc(b)

p <- 10
m <- 10
n <- m * p

set.seed(1)

data <- list(
  a = rep(1:p, each = m),
  b = rep(1:m, times = p),
  x = runif(n)
)

data$y <- data$x + rnorm(p, 0, 0.5)[data$a] + rnorm(m, 0, 0.3)[data$b] + rnorm(n, 0, 1)

prior <- make_prior(formula, data, family = "gaussian",
                    intercept_prior = c(0, 1000),
                    covariate_prior = list(x = c(0, 100)))

summary(prior)
plot_prior(prior)
plot_tree_structure(prior)

posterior <- makemyprior::inference_inla(prior)

makemyprior::plot_posterior_variance(posterior)

new_prior <- make_prior(
  formula, data,
  prior = list(
    tree = "s1 = (a, b); s2 = (s1, eps)",
    w = list(s1 = list(prior = "pcM", param = c(0.7, 0.5)),
             s2 = list(prior = "pc1", param = 0.75)),
    V = list(s2 = list(prior = "pc0", param = c(3, 0.05)))
  ),
  covariate_prior = list(x = c(0, 100))
)

summary(new_prior)
plot_prior(new_prior)
plot_tree_structure(new_prior)

new_posterior <- makemyprior::inference_inla(new_prior)

makemyprior::plot_posterior_variance(new_posterior)

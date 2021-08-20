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

posterior <- makemyprior::inference_inla(prior)
posterior2 <- makemyprior::inference_inla(new_prior)

# > posterior <- makemyprior::inference_inla(prior)
# Tree structure: a_b_eps = (a,b,eps)
#
# Weight priors:
#   (w[a/a_b_eps], w[b/a_b_eps]) ~ Dirichlet(3)
# Total variance priors:
#   V[a_b_eps] ~ Jeffreys'
#
# Error in `::`(base, quote) : could not find function "::"
# Error in `::`(base, quote) : could not find function "::"
# Error in `:::`(compiler, checkCompilerOptions) :
#   could not find function ":::"
# Fatal error: unable to initialize the JIT
#
# Error in inla.inlaprogram.has.crashed() :
#   The inla-program exited with an error. Unless you interupted it yourself, please rerun with verbose=TRUE and check the output carefully.
#   If this does not help, please contact the developers at <help@r-inla.org>.

sessionInfo()

# > sessionInfo()
# R version 4.1.0 (2021-05-18)
# Platform: x86_64-pc-linux-gnu (64-bit)
# Running under: Ubuntu 20.04.2 LTS
#
# Matrix products: default
# BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.9.0
# LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.9.0
#
# locale:
#   [1] LC_CTYPE=en_GB.UTF-8       LC_NUMERIC=C               LC_TIME=en_GB.UTF-8
# [4] LC_COLLATE=en_GB.UTF-8     LC_MONETARY=en_GB.UTF-8    LC_MESSAGES=en_GB.UTF-8
# [7] LC_PAPER=en_GB.UTF-8       LC_NAME=C                  LC_ADDRESS=C
# [10] LC_TELEPHONE=C             LC_MEASUREMENT=en_GB.UTF-8 LC_IDENTIFICATION=C
#
# attached base packages:
#   [1] parallel  stats     graphics  grDevices utils     datasets  methods   base
#
# other attached packages:
#   [1] stringr_1.4.0     purrr_0.3.4       tidyr_1.1.3       spdep_1.1-8       spData_0.3.8
# [6] sf_0.9-8          scales_1.1.1      readr_1.4.0       naomi_2.3.15      ggplot2_3.3.3
# [11] forcats_0.5.1     dplyr_1.0.6       INLA_21.02.23     sp_1.4-5          foreach_1.5.1
# [16] Matrix_1.3-4      makemyprior_1.0.0
#
# loaded via a namespace (and not attached):
#   [1] jsonlite_1.7.2     splines_4.1.0      gtools_3.9.2       shiny_1.5.0        assertthat_0.2.1
# [6] expm_0.999-6       yaml_2.2.1         LearnBayes_2.15.1  pillar_1.6.1       lattice_0.20-44
# [11] glue_1.4.2         digest_0.6.27      promises_1.1.1     traduire_0.0.6     colorspace_2.0-1
# [16] htmltools_0.5.1.1  httpuv_1.5.5       pkgconfig_2.0.3    raster_3.4-10      gmodels_2.18.1
# [21] xtable_1.8-4       gdata_2.18.0       later_1.1.0.1      MatrixModels_0.5-0 tibble_3.1.2
# [26] proxy_0.4-26       farver_2.1.0       generics_0.1.0     ellipsis_0.3.2     withr_2.4.2
# [31] shinyjs_2.0.0      magrittr_2.0.1     crayon_1.4.1       mime_0.10          deldir_0.2-10
# [36] fansi_0.5.0        nlme_3.1-152       MASS_7.3-54        class_7.3-19       tools_4.1.0
# [41] hms_1.1.0          lifecycle_1.0.0    V8_3.4.2           munsell_0.5.0      compiler_4.1.0
# [46] e1071_1.7-7        rlang_0.4.11       classInt_0.4-3     units_0.7-2        grid_4.1.0
# [51] iterators_1.0.13   rstudioapi_0.13    htmlwidgets_1.5.3  visNetwork_2.0.9   labeling_0.4.2
# [56] orderly_1.2.36     boot_1.3-28        gtable_0.3.0       codetools_0.2-18   DBI_1.1.1
# [61] curl_4.3.1         R6_2.5.0           fastmap_1.1.0      utf8_1.2.1         KernSmooth_2.23-20
# [66] stringi_1.6.2      Rcpp_1.0.6         vctrs_0.3.8        tidyselect_1.1.1   coda_0.19-4

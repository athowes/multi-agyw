#' Trying to gain intuition as to appropriate precision priors for multinomial models
#' Many random effects is a separate issue

library(tidyverse)

modified_softmax <- function(x, n = 1) {
  x <- c(rep(0, n), x)
  z <- exp(x) / sum(exp(x))
  z[n + 1] - z[n]
}

n_max <- 9
grid <- seq(-5, 5, by = 0.1)

df <- data.frame(
    x = rep(grid, n_max),
    n = rep(1:n_max, each = length(grid))
  ) %>%
  mutate(y = pmap_dbl(., modified_softmax))

pdf("softmax-intuition.pdf", h = 11, w = 8.5)

ggplot(df, aes(x = x, y = y)) +
  geom_line() +
  facet_wrap(~n)

dev.off()

#' What do the PC priors for the correlation parameter in an AR1 model look like?
#' Should I be using 0 or 1 as a base model?
#' More convinced by 1 being the right base model, don't want to be symmetric about 0

#' 1 as the base model
data.frame(x = seq(from = -1, to = 1, by = 0.01)) %>%
  mutate(y = INLA::inla.pc.dcor1(x, u = 0, alpha = 0.75)) %>%
  ggplot(aes(x = x, y = y)) +
  geom_line() +
  geom_vline(xintercept = 1, linetype = "dashed", alpha = 0.3) +
  labs(
    title = "PC prior for AR(1) correlation parameter",
    subtitle = "Base model is rho = 1 with P(rho > 0 = 0.75)",
    x = "rho", y = "p(rho)"
  ) +
  theme_minimal()

#' 0 as the base model
data.frame(x = seq(from = -1, to = 1, by = 0.01)) %>%
  mutate(y = INLA::inla.pc.dcor0(x, u = 0.5, alpha = 0.1)) %>%
  ggplot(aes(x = x, y = y)) +
  geom_line() +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.3) +
  labs(
    title = "PC prior for AR(1) correlation parameter",
    subtitle = "Base model is rho = 0 with P(rho > 0.5 = 0.1)",
    x = "rho", y = "p(rho)"
  ) +
  theme_minimal()

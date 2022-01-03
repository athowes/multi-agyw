set.seed(1)

data <- expand.grid(
  area = 1:4,
  time = 1:2,
  category = 1:3
)

data$y <- rpois(nrow(data), 2.5)

adj <- rbind(
  c(0, 1, 1, 0),
  c(0, 0, 1, 1),
  c(0, 0, 0, 0),
  c(0, 0, 0, 0)
)

adj <- adj + t(adj)
rownames(adj) <- colnames(adj) <- letters[1:4]


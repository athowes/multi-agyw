#' From https://haakonbakkagit.github.io/btopic120.html
precision_ar1 = function(N, rho){
  Q <- matrix(0, N, N)
  diag(Q) <- 1 + rho^2
  for (i in 1:(N-1)) {
    Q[i, i + 1] <- -rho
    Q[i + 1, i] <- -rho
  }
  Q[1, 1] <- 1
  Q[N, N] <- 1
  return(Q)
}

Q <- precision_ar1(3, 1)

exp(mean(log(diag(Q))))

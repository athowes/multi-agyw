#' Suppose that the ORs we calculate are
or <- c(1, 3.8, 3.7, 5.1)

#' And in log-odds ratio form
lor <- log(or)

#' 10% prevalence
prev <- 0.1

#' 100 people in the district
N <- 100

#' Broken down by behaviour as follows
N_fine <- c(30, 20, 45, 5)

#' Some first guess as to the prevalence in the subgroups
prev_fine <- c(0.05, 0.1, 0.1, 0.3)

#' On the logit scale
logit_prev_fine <- qlogis(prev_fine)

#' Values to aim for
target_val <- lor

#' Current values
current_val <- logit_prev_fine - logit_prev_fine[1]

#' Loss function
sum(current_val - target_val)^2

#' Want to constrain such that these two things are equal
prev * N
sum(prev_fine * N_fine)

#' Function to simulate random effects.
#'
#' Input simulation parameters and returns random effects.
#'
#' Simulates random effects for the master function \code{\link{sim_reg}} when
#' simulating a linear mixed model, both cross sectional and longitudinal. 
#' Allows the ability to simulate random effects from a Laplace, 
#' chi-square (1), mixture normal, or normal distribution.
#'
#' @param random_var Variance of random effects. Must be same length as random.
#' @param n Cluster sample size.
#' @param rand_gen The generating function used (e.g. rnorm).
#' @param ther A vector of length two that specifies the theoretical mean and
#'              standard deviation of the rand_gen. This would commonly be used
#'              to standardize the generating variable to have a mean of 0 and
#'              standard deviation of 1 to meet model assumptions. The variable
#'              is then rescaled to have the variance specified by random_var.
#' @param ther_sim A TRUE/FALSE flag indicating whether the error simulation 
#'  function should be simulated, that is should the mean and standard deviation
#'  used for standardization be simulated.
#' @param cor_vars A vector of correlations between random effects. 
#' @param ... Additional values that need to be passed to the function
#'             called from rand_gen.
#' @export
sim_rand_eff <- function(random_var, n, rand_gen, ther = c(0, 1),
                         ther_sim = FALSE, cor_vars = NULL, ...) {

  # Look to edit this with match.arg and switch functions
  if(ther_sim) {
    ther_val <- sapply(X = 1000000, FUN = rand_gen, ...)
    ther <- c(mean(ther_val), sd(ther_val))
  }
  
  reff <- do.call('cbind', lapply(seq_along(random_var), function(xx)  
    standardize(sapply(n, FUN = rand_gen, ...),
                mean = ther[1], sd = ther[2])))
  
  if(is.null(cor_vars)) {
    reff <- reff %*% chol(diag(length(random_var)) * random_var)
  } else {
    c_mat <- matrix(nrow = length(random_var), ncol = length(random_var))
    diag(c_mat) <- 1
    c_mat[upper.tri(c_mat)] <- c_mat[lower.tri(c_mat)] <- cor_vars
    cov <- diag(sqrt(random_var)) %*% 
      c_mat %*% diag(sqrt(random_var))
    es <- eigen(cov, symmetric = TRUE)
    ev <- es$values
    reff <- t(es$vectors %*% diag(sqrt(pmax(ev, 0)), length(random_var)) %*% 
                t(reff))
  }

 reff
}


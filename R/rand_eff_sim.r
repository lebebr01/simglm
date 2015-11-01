#' Function to simulate random effects.
#'
#' Input simulation parameters and returns random effects.
#'
#' Simulates random effects for the master function \code{\link{sim_reg}} when
#' simulating a linear mixed model, both cross sectional and longitudinal.  Allows
#' the ability to simulate random effects from a Laplace, chi-square (1), mixture
#' normal, or normal distribution.
#'
#' @param random.param Variance of random effects. Must be same length as random.
#' @param n Cluster sample size.
#' @param rand_gen The generating function used.
#' @param ther A vector of length two that specifies the theoretical mean and
#'              standard deviation of the rand_gen. This would commonly be used
#'              to standardize the generating variable to have a mean of 0 and
#'              standard deviation of 1 to meet model assumptions. The variable
#'              is then rescaled to have the variance specified by random.param.
#' @param ther_sim A TRUE/FALSE flag indicating whether the error simulation function
#'              should be simulated, that is should the mean and standard deviation
#'              used for standardization be simulated.
#' @param cor_vars A vector of correlations between random effects. 
#' @param ... Additional values that need to be passed to the function
#'             called from rand_gen.
#' @export
sim_rand_eff <- function(random.param, n, rand_gen, ther = c(0, 1),
                         ther_sim = FALSE, cor_vars = NULL, ...) {

  # Look to edit this with match.arg and switch functions
  if(ther_sim) {
    ther_val <- sapply(X = 1000000, FUN = rand_gen, ...)
    ther <- c(mean(ther_val), sd(ther_val))
  }
  
  reff <- sapply(1:length(random.param), function(xx)  
    standardize(sapply(n, FUN = eval(parse(text = rand_gen)), ...),
                mean = ther[1], sd = ther[2]))
  
  if(is.null(cor_vars) == FALSE) {
    c_mat <- matrix(nrow = length(random.param), ncol = length(random.param))
    diag(c_mat) <- 1
    c_mat[upper.tri(c_mat)] <- c_mat[lower.tri(c_mat)] <- cor_vars
    cov <- diag(sqrt(random.param)) %*% c_mat %*% diag(sqrt(random.param))
    reff <- reff %*% chol(cov)
  } 

 return(reff)
}

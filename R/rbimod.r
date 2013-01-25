#' Simulating mixture normal distributions
#' 
#' Input simulation metrics returns mixture normal random variable.
#' 
#' @param n Number of random draws.  Optionally can be a vector with number in each 
#' simulated normal distribution.
#' @param mean Vector of mean values for each normal distribution.
#' Must be the same length as num.dist.
#' @param var Vector of variance values for each normal distribution.
#' Must be the same length as num.dist.
#' @param num.dist Number of normal distributions to use when simulating mixture normal distribution.
rbimod <- function(n, mean, var, num.dist){
  if(length(mean) != num.dist) stop("length of mean must equal num.dist")
  if(length(var) != num.dist) stop("length of var must equal num.dist")
  
  if(length(n) > 1) {
    unlist(lapply(1:num.dist, function(xx) rnorm(n[xx], mean = mean[xx], sd = var[xx])))
  } else {
    unlist(lapply(1:num.dist, function(xx) rnorm(n/num.dist, mean = mean[xx], sd = var[xx])))
  }

}
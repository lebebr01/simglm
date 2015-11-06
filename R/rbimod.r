#' Simulating mixture normal distributions
#' 
#' Input simulation metrics returns mixture normal random variable.
#' 
#' Function to simulate mixture normal distributions.  The function computes
#' adds the specified number of normal distributions into a single vector.
#' 
#' Use of the function \code{\link{desireVar}} can be used to generate a mixture 
#' normal distribution with a specific global variance.
#' 
#' @param n Number of random draws.  Optionally can be a vector with number in each 
#' simulated normal distribution.
#' @param mean Vector of mean values for each normal distribution.
#' Must be the same length as num_dist.
#' @param var Vector of variance values for each normal distribution.
#' Must be the same length as num_dist.
#' @param num_dist Number of normal distributions to use when simulating mixture normal distribution.
#' @export 
#' @examples
#' \donttest{
#' ## mix normal with two normal distributions (bimodal)
#' simData <- rbimod(100, mean = c(-2, 3), var = c(1.5, 1.5), num_dist = 2)
#' plot(density(simData))
#' 
#' ## mixt normal with four distributions (multimodal)
#' simData <- rbimod(400, mean = c(-14, -4, 6, 20), var = c(rep(1.2, 4)), num_dist = 4)
#' plot(density(simData))
#' }
rbimod <- function(n, mean, var, num_dist){
  if(length(mean) != num_dist) stop("length of mean must equal num_dist")
  if(length(var) != num_dist) stop("length of var must equal num_dist")
  
  if(length(n) > 1) {
    unlist(lapply(1:num_dist, function(xx) rnorm(n[xx], mean = mean[xx], sd = var[xx])))
  } else {
    unlist(lapply(1:num_dist, function(xx) rnorm(n/num_dist, mean = mean[xx], sd = var[xx])))
  }

}

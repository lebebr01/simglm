#' Computes mixture normal variance
#' 
#' Input the desired variance, number of distributions, and mean of the 
#' distributions, returns a value of the variance of each mixture distribution.
#' 
#' This function can be used to generate the inputs for the \code{\link{rbimod}} 
#' variances when a specific variance is desired.  Especially useful when 
#' attempting to simulate a mixture normal/bimodal distribution.
#' 
#' @param desVar Desired overall variance of mixture normal distribution.
#' @param num_dist Number of normal distributions.
#' @param means Vector of means for each normal distribution. 
#'  Must equal num_dist.
#' @param equalWeight Should equal weights be used, 
#'  only TRUE is currently supported.
#' @export 
desireVar <- function(desVar, num_dist, means, equalWeight = TRUE){
  if(length(means) != num_dist) stop("num_dist must equal length of means")
  if(equalWeight != TRUE) stop("Only equalWeight is currently supported")
  mu <- mean(means)
  sigmasq <- (desVar/(1/num_dist) - sum((means-mu)^2))/num_dist
  if(sigmasq < 0){
    stop("Negative variance returned, add more distributions or change means")
  } else {
    sigmasq
  }
}

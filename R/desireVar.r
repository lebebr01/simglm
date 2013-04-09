#' Computes mixture normal variance for each distribution to have overall distribution.
#' 
#' Input the desired variance, number of distributions, and mean of the distributions,
#' returns a value of the variance of each mixture distribution.
#' 
#' @parm desVar Desired overall variance of mixture normal distribution.
#' @param num.dist Number of normal distributions.
#' @param means Vector of means for each normal distribution.  Must equal num.dist.
#' @param equalWeight Should equal weights be used, only TRUE is currently supported.
#' @export 
desireVar <- function(desVar, num.dist, means, equalWeight = TRUE){
  if(length(means) != num.dist) stop("num.dist must equal length of means")
  if(equalWeight != TRUE) stop("Only equalWeight is currently supported")
  mu <- mean(means)
  sigmasq <- (desVar/(1/num.dist) - sum((means-mu)^2))/num.dist
  if(sigmasq < 0){
    stop("Negative variance returned, add more distributions or change means")
  } else {
    sigmasq
  }
}
#' Simulates single level data
#' 
#' Takes simulation paramter arguments and 
#' returns simulated data.
#' 
#' @param Xmat A matrix of covariates.
#' @param beta A vector of regression parameters.
#' @param n Number of clusters.
#' @param err A vector of within cluster errors.
#' @export 
data.reg.single <- function(Xmat, beta, n, err) {

  Fbeta <-(Xmat %*% beta)  #Simulate average growth curve
  sim.data <- Fbeta + err  #Adding everything together
  sim.data <- cbind(Fbeta, err, sim.data)  
  colnames(sim.data) <- c("Fbeta", "err", "sim.data")
  sim.data
  
}
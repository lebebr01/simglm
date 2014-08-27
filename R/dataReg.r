#' Simulates nested data
#' 
#' Takes simulation paramter arguments and 
#' returns simulated data.
#' 
#' @param Xmat A matrix of covariates.
#' @param Zmat Design matrix for random effects.
#' @param beta A vector of regression parameters.
#' @param rand.eff A vector of random effects, must be stacked.
#' @param n Number of clusters.
#' @param p Number of units within each cluster.
#' @param err A vector of within cluster errors.
#' @importFrom Matrix bdiag
#' @export 
data.reg.nested <- function(Xmat, Zmat, beta, rand.eff, n, p, err) {
  
   Fbeta <- (Xmat %*% beta) 
    
    Zmat <- data.frame(Zmat, ID = rep(1:n, each = p))
    ZmatList <- lapply(1:n, function(xx) as.matrix(subset(Zmat, ID == xx, select = 1:(ncol(Zmat)-1))))
    ZmatBlock <- bdiag(ZmatList)
    reVec <- matrix(c(t(rand.eff)))
    re <- as.matrix(ZmatBlock %*% reVec)
    
    sim.data <- Fbeta + re + err
    sim.data <- cbind(Fbeta, re, err, sim.data)
    colnames(sim.data) <- c("Fbeta", "randEff", "err", "sim.data")
    sim.data
}


#' Simulates single level data
#' 
#' Takes simulation paramter arguments and returns simulated data.
#' 
#' This is a helper function to the master function \code{\link{sim.reg}}, 
#' this function does the actual simulation to return the data for single 
#' level models.
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

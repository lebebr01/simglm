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
#' @import Matrix
#' @export 
data.reg.nested <- function(Xmat, Zmat, beta, rand.eff, n, p, err) {
  
   Fbeta <- (Xmat %*% beta) 
    
    Zmat <- cbind(Zmat, ID = rep(1:n, each = p))
    Zmatd <- data.frame(Zmat)
    ZmatList <- lapply(1:n, function(xx) as.matrix(subset(Zmatd, ID == xx, select = 1:(ncol(Zmatd)-1))))
    ZmatBlock <- bdiag(ZmatList)
    reVec <- matrix(c(t(rand.eff)))
    re <- as.matrix(ZmatBlock %*% reVec)
    
    sim.data <- Fbeta + re + err
    sim.data <- cbind(Fbeta, re, err, sim.data)
    colnames(sim.data) <- c("Fbeta", "randEff", "err", "sim.data")
    sim.data
}
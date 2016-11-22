#' Simulates single level data
#' 
#' Takes simulation parameter arguments and returns simulated data.
#' 
#' This is a helper function to the master function \code{\link{sim_reg}}, 
#' this function does the actual simulation to return the data for single 
#' level models.
#' 
#' @param Xmat A matrix of covariates.
#' @param beta A vector of regression parameters.
#' @param n Number of clusters.
#' @param err A vector of within cluster errors.
#' @export 
data_reg_single <- function(Xmat, beta, n, err) {
  
  Fbeta <-(Xmat %*% beta)  #Simulate average growth curve
  sim_data <- Fbeta + err  #Adding everything together
  sim_data <- cbind(Fbeta, err, sim_data)  
  colnames(sim_data) <- c("Fbeta", "err", "sim_data")
  return(sim_data)
  
}

#' Simulates two level nested data
#' 
#' Takes simulation parameter arguments and 
#' returns simulated data.
#' 
#' @param Xmat A matrix of covariates.
#' @param Zmat Design matrix for random effects.
#' @param beta A vector of regression parameters.
#' @param rand_eff A vector of random effects, must be stacked.
#' @param n Number of clusters.
#' @param p Number of units within each cluster.
#' @param err A vector of within cluster errors.
#' @importFrom Matrix bdiag
#' @export 
data_reg_nested <- function(Xmat, Zmat, beta, rand_eff, n, p, err) {
  
   Fbeta <- (Xmat %*% beta) 
    
    ID <- NULL
    Zmat <- data.frame(Zmat, ID = rep(1:n, times = p))
    ZmatList <- lapply(1:n, function(xx) 
      as.matrix(subset(Zmat, ID == xx, select = 1:(ncol(Zmat)-1))))
    ZmatBlock <- bdiag(ZmatList)
    reVec <- matrix(c(t(rand_eff)))
    re <- as.matrix(ZmatBlock %*% reVec)

    sim_data <- Fbeta + re + err
    sim_data <- cbind(Fbeta, re, err, sim_data)
    colnames(sim_data) <- c("Fbeta", "randEff", "err", "sim_data")
    return(sim_data)
}

#' Simulates three level nested data with a single third level random effect
#' 
#' Takes simulation parameter arguments and 
#' returns simulated data.
#' 
#' @param Xmat A matrix of covariates.
#' @param Zmat Design matrix for random effects.
#' @param Zmat3 Design matrix for level 3 random effects.
#' @param beta A vector of regression parameters.
#' @param rand_eff A vector of random effects, must be stacked.
#' @param rand_eff3 A vector of level 3 random effects, must be stacked.
#' @param k Number of third level clusters.
#' @param n Number of clusters.
#' @param p Number of units within each cluster.
#' @param err A vector of within cluster errors.
#' @importFrom Matrix bdiag
#' @export 
data_reg_nested3 <- function(Xmat, Zmat, Zmat3, beta, rand_eff, rand_eff3, 
                             k, n, p, err) {
  
  
  end <- cumsum(n)
  beg <- c(1, cumsum(n) + 1)
  beg <- beg[-length(beg)]
  
  lvl3ss <- sapply(lapply(1:length(beg), function(xx) 
    p[beg[xx]:end[xx]]), sum)
  
  Fbeta <- (Xmat %*% beta) 
  
  ID <- NULL
  Zmat <- data.frame(Zmat, ID = rep(1:length(p), times = p))
  ZmatList <- lapply(1:length(p), function(xx) 
    as.matrix(subset(Zmat, ID == xx, select = 1:(ncol(Zmat) - 1))))
  ZmatBlock <- bdiag(ZmatList)
  reVec <- matrix(c(t(rand_eff)))
  re <- as.matrix(ZmatBlock %*% reVec)
  
  Zmat3 <- data.frame(Zmat3, ID = rep(1:k, times = lvl3ss))
  Zmat3List <- lapply(1:k, function(xx) 
    as.matrix(subset(Zmat3, ID == xx, select = 1:(ncol(Zmat3) - 1))))
  Zmat3Block <- bdiag(Zmat3List)
  re3Vec <- as.matrix(c(t(rand_eff3)))
  re3 <- as.matrix(Zmat3Block %*% re3Vec)
  
  sim_data <- Fbeta + re + re3 + err
  sim_data <- cbind(Fbeta, re, re3, err, sim_data)
  colnames(sim_data) <- c("Fbeta", "randEff", "randEff3", "err", "sim_data")
  return(sim_data)
}

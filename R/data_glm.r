#' Generate logistic regression outcome
#' 
#' Takes simulation parameter arguments and 
#' returns simulated data in the logistic metric and
#' converted to 0/1 based on the probabilities and the 
#' binomial distribution with 1 trial.
#' 
#' @param Xmat A matrix of covariates.
#' @param beta A vector of regression parameters.
#' @param n Number of clusters.
#' @export
data_glm_single <- function(Xmat, beta, n) {
  
  Fbeta <-(Xmat %*% beta)
  logistic <- exp(Fbeta)/(1 + exp(Fbeta))
  sim_data <- rbinom(n, 1, logistic)
  sim.data <- data.frame(Fbeta = Fbeta, logistic = logistic,
                         sim.data = sim_data)
  return(sim.data)
}


#' Generate logistic regression outcome
#' 
#' Takes simulation parameter arguments and 
#' returns simulated data in the logistic metric and
#' converted to 0/1 based on the probabilities and the 
#' binomial distribution with 1 trial.
#' 
#' @param Xmat A matrix of covariates.
#' @param Zmat Design matrix for random effects.
#' @param beta A vector of regression parameters.
#' @param rand.eff A vector of random effects, must be stacked.
#' @param n Number of clusters.
#' @param p Number of units within each cluster.
#' @importFrom Matrix bdiag
#' @export
data_glm_nested <- function(Xmat, Zmat, beta, rand.eff, n, p) {
  
  Fbeta <- (Xmat %*% beta) 
  
  ID <- NULL
  Zmat <- data.frame(Zmat, ID = rep(1:n, times = p))
  ZmatList <- lapply(1:n, function(xx) as.matrix(subset(Zmat, ID == xx, 
                                        select = 1:(ncol(Zmat)-1))))
  ZmatBlock <- bdiag(ZmatList)
  reVec <- matrix(c(t(rand.eff)))
  re <- as.matrix(ZmatBlock %*% reVec)
  
  logistic <- Fbeta + re
  prob <- exp(logistic)/(1 + exp(logistic))
  sim_data <- rbinom(length(prob), 1, prob)
  sim.data <- cbind(Fbeta, re, logistic, prob, sim_data)
  colnames(sim.data) <- c("Fbeta", "randEff", 'logistic', 'prob', "sim.data")
  return(sim.data)
}

#' Simulates three level nested data with a single third level random effect
#' 
#' Takes simulation parameter arguments and 
#' returns simulated data in the logistic metric and
#' converted to 0/1 based on the probabilities and the 
#' binomial distribution with 1 trial.
#' 
#' @param Xmat A matrix of covariates.
#' @param Zmat Design matrix for random effects.
#' @param Zmat3 Design matrix for level 3 random effects.
#' @param beta A vector of regression parameters.
#' @param rand.eff A vector of random effects, must be stacked.
#' @param rand.eff3 A vector of level 3 random effects, must be stacked.
#' @param k Number of third level clusters.
#' @param n Number of clusters.
#' @param p Number of units within each cluster.
#' @importFrom Matrix bdiag
#' @export 
data_glm_nested3 <- function(Xmat, Zmat, Zmat3, beta, rand.eff, rand.eff3, k, n, p) {
  
  
  end <- cumsum(n)
  beg <- c(1, cumsum(n) + 1)
  beg <- beg[-length(beg)]
  
  lvl3ss <- sapply(lapply(1:length(beg), function(xx) 
    p[beg[xx]:end[xx]]), sum)
  
  Fbeta <- (Xmat %*% beta) 
  
  ID <- NULL
  Zmat <- data.frame(Zmat, ID = rep(1:length(p), times = p))
  ZmatList <- lapply(1:length(p), function(xx) as.matrix(subset(Zmat, ID == xx, select = 1:(ncol(Zmat) - 1))))
  ZmatBlock <- bdiag(ZmatList)
  reVec <- matrix(c(t(rand.eff)))
  re <- as.matrix(ZmatBlock %*% reVec)
  
  Zmat3 <- data.frame(Zmat3, ID = rep(1:k, times = lvl3ss))
  Zmat3List <- lapply(1:k, function(xx) as.matrix(subset(Zmat3, ID == xx, select = 1:(ncol(Zmat3) - 1))))
  Zmat3Block <- bdiag(Zmat3List)
  re3Vec <- as.matrix(c(t(rand.eff3)))
  re3 <- as.matrix(Zmat3Block %*% re3Vec)
  
  logistic <- Fbeta + re + re3
  prob <- exp(logistic)/(1 + exp(logistic))
  sim_data <- rbinom(length(prob), 1, prob)
  sim.data <- cbind(Fbeta, re, re3, logistic, prob, sim_data)
  colnames(sim.data) <- c("Fbeta", "randEff", 'randEff3', 'logistic', 
                          'prob', "sim.data")
  return(sim.data)
}

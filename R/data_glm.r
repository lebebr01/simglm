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

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
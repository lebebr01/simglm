#' Generate logistic regression outcome
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
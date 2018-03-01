#' Generate logistic regression outcome
#' 
#' Takes simulation parameter arguments and 
#' returns simulated data for two different probability distributions. 
#' One is logistic (0/1) outcome and the second being poisson (count) outcomes.
#' 
#' @param Xmat A matrix of covariates.
#' @param beta A vector of regression parameters.
#' @param n Number of clusters.
#' @param outcome_type A vector specifying the type of outcome, must be either
#'   logistic or poisson. Logitstic outcome will be 0/1 and poisson outcome will
#'   be counts.
#' @export
data_glm_single <- function(Xmat, beta, n, 
                            outcome_type) {
  if(outcome_type == 'logistic') {
    Fbeta <-(Xmat %*% beta)
    logistic <- exp(Fbeta)/(1 + exp(Fbeta))
    sim_data <- rbinom(n, 1, logistic)
    sim_data <- data.frame(Fbeta = Fbeta, logistic = logistic,
                           sim_data = sim_data)
  } else {
    Fbeta <-(Xmat %*% beta)
    pois <- exp(Fbeta)
    sim_data <- rpois(n, pois)
    sim_data <- data.frame(Fbeta = Fbeta, poisson = pois,
                           sim_data = sim_data)
  }
  
  sim_data
}


#' Generate logistic regression outcome
#' 
#' Takes simulation parameter arguments and 
#' returns simulated data for two different probability distributions. 
#' One is logistic (0/1) outcome and the second being poisson (count) outcomes.
#' 
#' @param Xmat A matrix of covariates.
#' @param Zmat Design matrix for random effects.
#' @param beta A vector of regression parameters.
#' @param rand_eff A vector of random effects, must be stacked.
#' @param n Number of clusters.
#' @param p Number of units within each cluster.
#' @param outcome_type A vector specifying the type of outcome, must be either
#'   logistic or poisson. Logitstic outcome will be 0/1 and poisson outcome will
#'   be counts.
#' @importFrom Matrix bdiag
#' @export
data_glm_nested <- function(Xmat, Zmat, beta, rand_eff, n, p, 
                            outcome_type) {
  
  Fbeta <- (Xmat %*% beta) 
  
  ID <- NULL
  Zmat <- data.frame(Zmat, ID = rep(1:n, times = p))
  ZmatList <- lapply(1:n, function(xx) as.matrix(subset(Zmat, ID == xx, 
                                        select = 1:(ncol(Zmat)-1))))
  ZmatBlock <- bdiag(ZmatList)
  reVec <- matrix(c(t(rand_eff)))
  re <- as.matrix(ZmatBlock %*% reVec)
  
  if(outcome_type == 'logistic') {
    logistic <- Fbeta + re
    prob <- exp(logistic)/(1 + exp(logistic))
    sim_data <- rbinom(length(prob), 1, prob)
    sim_data <- cbind(Fbeta, re, logistic, prob, sim_data)
    colnames(sim_data) <- c("Fbeta", "randEff", 'logistic', 'prob', "sim_data")
  } else {
    log_out <- Fbeta + re
    pois <- exp(log_out)
    sim_data <- rpois(length(pois), pois)
    sim_data <- cbind(Fbeta, re, log_out, pois, sim_data)
    colnames(sim_data) <- c("Fbeta", "randEff", 'log_out', 'poisson', "sim_data")
  }
  
  sim_data
}

#' Simulates three level nested data with a single third level random effect
#' 
#' Takes simulation parameter arguments and 
#' returns simulated data for two different probability distributions. 
#' One is logistic (0/1) outcome and the second being poisson (count) outcomes.
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
#' @param outcome_type A vector specifying the type of outcome, must be either
#'   logistic or poisson. Logitstic outcome will be 0/1 and poisson outcome will
#'   be counts.
#' @importFrom Matrix bdiag
#' @export 
data_glm_nested3 <- function(Xmat, Zmat, Zmat3, beta, rand_eff, rand_eff3, 
                             k, n, p, outcome_type) {
  
  
  end <- cumsum(n)
  beg <- c(1, cumsum(n) + 1)
  beg <- beg[-length(beg)]
  
  lvl3ss <- sapply(lapply(1:length(beg), function(xx) 
    p[beg[xx]:end[xx]]), sum)
  
  Fbeta <- (Xmat %*% beta) 
  
  ID <- NULL
  Zmat <- data.frame(Zmat, ID = rep(1:length(p), times = p))
  ZmatList <- lapply(1:length(p), function(xx) as.matrix(subset(Zmat, ID == xx, 
                                                select = 1:(ncol(Zmat) - 1))))
  ZmatBlock <- bdiag(ZmatList)
  reVec <- matrix(c(t(rand_eff)))
  re <- as.matrix(ZmatBlock %*% reVec)
  
  Zmat3 <- data.frame(Zmat3, ID = rep(1:k, times = lvl3ss))
  Zmat3List <- lapply(1:k, function(xx) as.matrix(subset(Zmat3, ID == xx, 
                                                select = 1:(ncol(Zmat3) - 1))))
  Zmat3Block <- bdiag(Zmat3List)
  re3Vec <- as.matrix(c(t(rand_eff3)))
  re3 <- as.matrix(Zmat3Block %*% re3Vec)
  
  if(outcome_type == 'logistic') {
    logistic <- Fbeta + re + re3
    prob <- exp(logistic)/(1 + exp(logistic))
    sim_data <- rbinom(length(prob), 1, prob)
    sim_data <- cbind(Fbeta, re, re3, logistic, prob, sim_data)
    colnames(sim_data) <- c("Fbeta", "randEff", 'randEff3', 'logistic', 
                            'prob', "sim_data")
  } else {
    log_out <- Fbeta + re + re3
    pois <- exp(log_out)
    sim_data <- rpois(length(pois), pois)
    sim_data <- cbind(Fbeta, re, re3, log_out, pois, sim_data)
    colnames(sim_data) <- c("Fbeta", "randEff", 'randEff3', 'log_out', 
                            'poisson', "sim_data")
  }
  
  sim_data
}

#' Transform response variable
#' 
#' @param outcome The outcome variable to transform.
#' @param type Type of transformation to apply.
#' @param ... Additional arguments passed to distribution functions.
#' 
#' @export
transform_outcome <- function(outcome, type, ...) {
  
  if(type == 'logistic') {
    rbinom(length(outcome), size = 1, 
           prob = (exp(outcome) / (1 + exp(outcome))))
  }
  if(type == 'count') {
    rpois(length(outcome), lambda = exp(outcome))
  }
  if(type %ni% c('logistic', 'count')) {
    purrr::map()
  }
  
}


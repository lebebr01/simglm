<<<<<<< Updated upstream:R/randEffSim.r
#' Function to simulate random effects.
#' 
#' Input simulation parameters and returns random effects.
#' 
#' Simulates random effects for the master function \code{\link{sim.reg}} when
#' simulating a linear mixed model, both cross sectional and longitudinal.  Allows
#' the ability to simulate random effects from a Laplace, chi-square (1), mixture 
#' normal, or normal distribution.
#' 
#' @param random.param Variance of random effects. Must be same length as random.
#' @param cor Correlation between random effects.
#' @param n Cluster sample size.
#' @param dist Simulated random effect distribution.  Must be "lap", "chi", "norm", "bimod", 
#' "norm" is default.
#' @param num.dist Number of distributions for bimod random variables
#' @importFrom MASS mvrnorm 
#' @importFrom VGAM rlaplace
#' @export 
sim.rand.eff <- function(random.param, cor, n, dist = c("lap","chi","norm", "bimod"), num.dist){

  # Look to edit this with match.arg and switch functions
   
  if(dist == "lap"){ 

    reff <- do.call("cbind", lapply(1:length(random.param), function(xx) rlaplace(n, 0, 1)))
    c <- varcov.randeff(random.param, cor)
    reff1 <- reff %*% chol(c/2)

  }
  if(dist == "chi"){ 
      
    reff <- do.call("cbind", lapply(1:length(random.param), function(xx) rchisq(n, 1)))
    reff <- reff-1
    c <- varcov.randeff(random.param, cor)
    reff1 <- reff %*% chol(c/2)

  }
  if(dist == "bimod"){
        
    reff <- do.call("cbind", lapply(1:length(random.param), function(xx) 
      rbimod(n, mean = rep(0, num.dist), var = rep(1, num.dist), num.dist)))
    c <- varcov.randeff(random.param, cor)
    reff1 <- reff %*% chol(c/2)

  }
  if(dist == "norm"){
    c <- varcov.randeff(random.param, cor)
    reff1 <- mvrnorm(n, rep.int(0, length(random.param)), c)
  }
 return(reff1)  
}


#' Function to simulate third level random effects.
#' 
#' Input simulation parameters and returns random effects.
#' 
#' Simulates random effects for the master function \code{\link{sim.reg}} when
#' simulating a linear mixed model, both cross sectional and longitudinal.  Allows
#' the ability to simulate random effects from a Laplace, chi-square (1), mixture 
#' normal, or normal distribution.
#' 
#' @param random.param3 Variance of random effects. Currently only supports a single random effect for third level.
#' @param cor Correlation between level 3 random effects.
#' @param k Number of third level clusters.
#' @export 
sim.rand.eff3 <- function(random.param3, cor, k){

  c <- varcov.randeff(random.param3, cor)
  reff1 <- mvrnorm(k, rep.int(0, length(random.param3)), c)
  
  return(reff1)

}
=======
#' Function to simulate random effects.
#' 
#' Input simulation parameters and returns random effects.
#' 
#' Simulates random effects for the master function \code{\link{sim_reg}} when
#' simulating a linear mixed model, both cross sectional and longitudinal.  Allows
#' the ability to simulate random effects from a Laplace, chi-square (1), mixture 
#' normal, or normal distribution.
#' 
#' @param random.param Variance of random effects. Must be same length as random.
#' @param cor Correlation between random effects.
#' @param n Cluster sample size.
#' @param rand_gen The generating function used.
#' @param arima TRUE/FALSE flag indicating whether residuals should 
#'             be correlated. If TRUE, must specify a valid model to pass to 
#'             arima.sim, See \code{\link{arima.sim}} for examples.
#' @param ther A vector of length two that specifies the theoretical mean and 
#'              standard deviation of the rand_gen. This would commonly be used
#'              to standardize the generating variable to have a mean of 0 and
#'              standard deviation of 1 to meet model assumptions. The variable
#'              is then rescaled to have the variance specified by error_var.
#' @param ther_sim A TRUE/FALSE flag indicating whether the error simulation function
#'              should be simulated, that is should the mean and standard deviation
#'              used for standardization be simulated.
#' @param ... Additional values that need to be passed to the function
#'             called from rand_gen.
#' @export 
sim_rand_eff <- function(random.param, cor, n, rand_gen, ther, 
                         ther_sim = FALSE, ...){

  # Look to edit this with match.arg and switch functions
  if(ther_sim) {
    ther_val <- sapply(X = 1000000, FUN = rand_gen, ...)
    ther <- c(mean(ther_val), sd(ther_val))
  }
  
  match <- match.call()
  
  if(rand_gen != MASS::mvrnorm) {
    
  }
  
  if(dist == "lap"){ 

    reff <- do.call("cbind", lapply(1:length(random.param), function(xx) rlaplace(n, 0, 1)))
    c <- varcov_randeff(random.param, cor)
    reff1 <- reff %*% chol(c/2)

  }
  if(dist == "chi"){ 
      
    for(i in 1:10000) {
      reff <- do.call("cbind", lapply(1:length(random.param), function(xx) rchisq(n, 5)))
      reff <- (reff-5)
      c <- varcov_randeff(random.param, cor_re)
      reff1 <- reff %*% chol(c/10)
      tmp[i, ] <- c(sd(reff1[, 1]), sd(reff1[, 2]), cor(reff1[, 1], reff1[, 2]))
    }
    

  }
  if(dist == "bimod"){
        
    reff <- do.call("cbind", lapply(1:length(random.param), function(xx) 
      rbimod(n, mean = rep(0, num.dist), var = rep(1, num.dist), num.dist)))
    c <- varcov_randeff(random.param, cor)
    reff1 <- reff %*% chol(c/2)

  }
  if(dist == "norm"){
    c <- varcov_randeff(random.param, cor)
    reff1 <- mvrnorm(n, rep.int(0, length(random.param)), c)
  }
 return(reff1)  
}


#' Function to simulate third level random effects.
#' 
#' Input simulation parameters and returns random effects.
#' 
#' Simulates random effects for the master function \code{\link{sim_reg}} when
#' simulating a linear mixed model, both cross sectional and longitudinal.  Allows
#' the ability to simulate random effects from a Laplace, chi-square (1), mixture 
#' normal, or normal distribution.
#' 
#' @param random.param3 Variance of random effects. Currently only supports a single random effect for third level.
#' @param cor Correlation between level 3 random effects.
#' @param k Number of third level clusters.
#' @export 
sim_rand_eff3 <- function(random.param3, cor, k){

  c <- varcov_randeff(random.param3, cor)
  reff1 <- mvrnorm(k, rep.int(0, length(random.param3)), c)
  
  return(reff1)

}
>>>>>>> Stashed changes:R/rand_eff_sim.r

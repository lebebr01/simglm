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

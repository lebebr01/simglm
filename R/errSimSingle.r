#' Function that simulates errors.
#' 
#' Input error simulation parameters and outputs simulated errors.
#' 
#' Simulates error term for single level regression models.
#' 
#' @param errorVar Scalar of error variance
#' @param n Cluster sample size.
#' @param err.dist Simulated within cluster error distribution. Must be "lap", "chi", "norm", "bimod", 
#' "norm" is default.
#' @param num.dist Number of distributions for bimod random variables.
#' @export 
err.sim.single <- function(errorVar, n, err.dist, num.dist){
  
  require(MASS)
  
  if(err.dist == "norm"){
    err <- rnorm(n, 0, sd = sqrt(errorVar))
  }
  if(err.dist == "lap"){
    require(VGAM)
    err <- rlaplace(n,0,1)*chol((errorVar/2))
  }
  if(err.dist == "chi"){
    err <- ((rchisq(n,1)-1)/sqrt(2))*sqrt(errorVar)
  }
  if(err.dist == "bimod"){
    err <- rbimod(n, mean = rep(0, num.dist), var = rep(1, num.dist), num.dist)*chol((errorVar/2))
  }
  err
}

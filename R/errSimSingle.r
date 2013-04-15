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
#' @export 
err.sim.single <- function(errorVar, n, err.dist){
  
  require(MASS)
  
  if(err.dist == "norm"){
    err <- rnorm(n, 0, sd = sqrt(errorVar))
  } else {
    if(err.dist == "lap"){
      require(VGAM)
      err <- rlaplace(n,0,1)*chol((errorVar/2))
    } else {
      if(err.dist == "chi"){
        err <- ((rchisq(n,1)-1)/sqrt(2))*sqrt(errorVar)
      } else {
        #Note this does bimodal distribution with mean 0 and variance approx .64.
        #Does not change with errorVar
        err <- unlist(lapply(1:n, function(x){ c(rnorm(p/2,mean=.7,sd=.4),rnorm(p/2,mean=-.7,sd=.4))}))
      }
    }
  }
  err
}
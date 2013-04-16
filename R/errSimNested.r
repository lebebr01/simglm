#' Function that simulates errors.
#' 
#' Input error simulation parameters and outputs simulated errors.
#' 
#' @param errorVar Scalar of error variance
#' @param n Cluster sample size.
#' @param p Within cluster sample size.
#' @param serCor Simulation of serial correlation. Must be "AR", "MA", "ARMA", or "ID", "ID" is default.
#' @param serCorVal Serial correlation parameters. A list of values to pass on to arima.sim.
#' @param err.dist Simulated within cluster error distribution. Must be "lap", "chi", "norm", "bimod", 
#' "norm" is default.
#' @parm num.dist Number of distributions for bimod random variables
#' @export 
err.sim.nested <- function(errorVar, n, p, serCor, serCorVal, err.dist, num.dist){
  require(VGAM)
  require(MASS)
  if(serCor == "ARMA" & length(serCorVal) < 2) stop("Incorrect dimensions serCorVal")
  if(err.dist == "norm"){
    if(serCor == "AR"){
      err <- unlist(lapply(1:n,function(x){arima.sim(serCorVal,p, sd = sqrt(errorVar))}))
    } else {
      if(serCor == "MA"){
        err <- unlist(lapply(1:n,function(x){arima.sim(serCorVal,p, sd = sqrt(errorVar))}))
      } else {
        if(serCor == "ARMA"){
          err <- unlist(lapply(1:n,function(x){arima.sim(serCorVal,p, sd = sqrt(errorVar))}))
        } else {
          # generate multivariate normal error terms with zero mean 
          d2 <- (errorVar)*diag(p) 
          err <- matrix(c(mvrnorm(n,rep(0,p),d2)) ,nrow=n*p, ncol = 1)
        }
      }
    }
  }
  
  if(err.dist == "lap"){
    err <- unlist(lapply(1:n, function(x){rlaplace(p,0,1)*chol((errorVar/2))}))
  }
  
  if(err.dist == "chi"){
    err <- unlist(lapply(1:n, function(x){((rchisq(p,1)-1)/sqrt(2))*sqrt(errorVar)}))
  }
  
  if(err.dist == "bimod"){
    err <- unlist(lapply(1:n, function(x) {
      ((rbimod(p, mean = rep(0, num.dist), var = rep(1, num.dist), num.dist)
      *chol((errorVar/2)))) }))
  }
err
}

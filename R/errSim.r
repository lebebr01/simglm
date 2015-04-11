#' Function that simulates errors.
#' 
#' Input error simulation parameters and outputs simulated errors.
#' 
#' @param errorVar Scalar of error variance
#' @param n Cluster sample size.
#' @param p Within cluster sample size.
#' @param serCor Simulation of serial correlation. Must be "AR", "MA", "ARMA", or "ID", "ID" is default.
#' @param serCorVal Serial correlation parameters. A list of values to pass on to arima.sim.
#' @param err.dist Simulated within cluster error distribution. Must be "lap", "chi", "norm", 
#' "norm" is default.
#' @importFrom VGAM rlaplace 
#' @importFrom MASS mvrnorm
#' @export 
sim.err.nested <- function(errorVar, n, p, serCor, serCorVal, err.dist){
  
  # Look to edit this with match.arg and switch
  #n <- length(p)

  if(serCor == "ARMA" & length(serCorVal) < 2) stop("Incorrect dimensions serCorVal")
  if(err.dist == "norm"){
    if(serCor == "AR"){
      err <- unlist(lapply(1:n, function(x) {
        arima.sim(serCorVal, p[x], sd = sqrt(errorVar))
        }))
    } else {
      if(serCor == "MA"){
        err <- unlist(lapply(1:n, function(x) {
          arima.sim(serCorVal, p[x], sd = sqrt(errorVar))
          }))
      } else {
        if(serCor == "ARMA"){
          err <- unlist(lapply(1:n, function(x) {
            arima.sim(serCorVal, p[x], sd = sqrt(errorVar))
            }))
        } else {
          # generate multivariate normal error terms with zero mean 
          #d2 <- (errorVar)*diag(p) 
          err <- unlist(lapply(1:length(p), function(xx) 
            mvrnorm(n = 1,rep(0,p[xx]), Sigma = (errorVar) * diag(p[xx]))))
        }
      }
    }
  }
  
  if(err.dist == "lap"){
    err <- unlist(lapply(1:n, function(x){rlaplace(p,0,1)*chol(errorVar/2)}))
  }
  
  if(err.dist == "chi"){
    err <- unlist(lapply(1:n, function(x){(rchisq(p,1)-1)*chol(errorVar/2)}))
  }
  
#   if(err.dist == "bimod"){
#     err <- unlist(lapply(1:n, function(x) {
#       rbimod(p, mean, var, num.dist) }))
#   }
err
}


#' Function that simulates errors.
#' 
#' Input error simulation parameters and outputs simulated errors.
#' 
#' Simulates error term for single level regression models.
#' 
#' @param errorVar Scalar of error variance
#' @param n Cluster sample size.
#' @param err.dist Simulated within cluster error distribution. Must be "lap", "chi", "norm", 
#' "norm" is default.
#' @importFrom VGAM rlaplace
#' @export 
sim.err.single <- function(errorVar, n, err.dist){
  
  if(err.dist == "norm"){
    err <- rnorm(n, 0, sd = sqrt(errorVar))
  }
  if(err.dist == "lap"){
    err <- rlaplace(n,0,1)*chol(errorVar/2)
  }
  if(err.dist == "chi"){
    err <- (rchisq(n,1)-1)*chol(errorVar/2)
  }
#   if(err.dist == "bimod"){
#     err <- rbimod(n, mean, var, num.dist)
#   }
  err
}

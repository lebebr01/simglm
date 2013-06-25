#' Simulates design matrix.
#' 
#' Input fixed variables, sample size, and number of within variables, returns design matrix.
#' 
#' Simulates the fixed effects for the \code{\link{sim.reg}} function when a linear mixed
#' model is specified.  This function assumes a time variable when longitudinal data 
#' is specified and does include any interactions that are specified.
#' 
#' @param fixed One sided formula for fixed effects in the simulation, currently assumes intercept.
#' @param fixed.vars Character vector of covariates for design matrix.
#' @param cov.param List of mean and variance for fixed effects. Does not include intercept, time, or 
#' interactions. Must be same order as fixed formula above.
#' @param n Number of clusters.
#' @param p Number of within cluster units.
#' @param w.var Number of time varying covariates or level one covariates for cross-sectional clustering.  
#' This number includes the intercept and time variable for longitudinal data.
#' @param data.str Type of data. Must be "cross", or "long".
#' @export 
sim.fixef.nested <- function(fixed, fixed.vars, cov.param, n, p, w.var, data.str){
  
  n.vars <- length(fixed.vars)
  n.int <- length(grep(":",fixed.vars))
  
  Xmat <- matrix(nrow=n*p,ncol = ifelse(w.var == 1, 0, 1))

  if(data.str == "long"){
    if(w.var == 2){
      Xmat[,1] <- rep.int((1:p)-1,times = n)
    } else { 
      Xmat[,1] <- rep.int((1:p)-1,times = n)
      Xmat <- cbind(Xmat, do.call("cbind", lapply(3:w.var, function(xx) 
        rnorm(n * p, mean = cov.param[[xx-2]][1], sd = cov.param[[xx-2]][2]))))
      }
    } else {
       Xmat <- do.call("cbind", lapply(2:w.var, function(xx) 
         rnorm(n * p, mean = cov.param[[xx-1]][1], sd = cov.param[[xx-1]][2])))
     }
  
  
  if(n.int == 0){
    if(w.var != n.vars+1){
      Xmat <- cbind(Xmat, do.call("cbind", lapply((w.var+1):(n.vars+1), function(xx)
        rep(rnorm(n, mean = cov.param[[xx-2]][1], sd=cov.param[[xx-2]][2]), each = p))))
    } 
  } else {
    num.no.int <- n.vars - n.int                  
    if(w.var != num.no.int+1){
      Xmat <- cbind(Xmat, do.call("cbind", lapply((w.var+1):(num.no.int+1), function(xx)
        rep(rnorm(n, mean = cov.param[[xx-2]][1], sd=cov.param[[xx-2]][2]), each = p))))
    }
  }  

   if(n.int == 0){
     colnames(Xmat) <- fixed.vars
   } else {
     int.loc <- grep(":", fixed.vars)
     colnames(Xmat) <- fixed.vars[-int.loc]
   } 
 Xmat <- model.matrix(fixed, data.frame(Xmat))
 Xmat
}


#' Simulates design matrix for single level model.
#' 
#' Input fixed variables, sample size, and number of within variables, returns design matrix.
#' 
#' Simulates the fixed effects for the \code{\link{sim.reg}} function when simulating a 
#' simple regression model.
#' 
#' @param fixed One sided formula for fixed effects in the simulation, currently assumes intercept.
#' @param fixed.vars Character vector of covariates for design matrix.
#' @param n Number of clusters.
#' @param cov.param List of mean and variance for fixed effects. Does not include intercept, time, or 
#' interactions. Must be same order as fixed formula above.
#' @export 
sim.fixef.single <- function(fixed, fixed.vars, n, cov.param){
  
  n.vars <- length(fixed.vars)
  n.int <- length(grep(":",fixed.vars))
  
  Xmat <- do.call("cbind", lapply(2:((n.vars - n.int)+1), function(xx)
    rnorm(n, mean = cov.param[[xx-1]][1], sd = cov.param[[xx-1]][2])))
  
  if(n.int == 0){
    colnames(Xmat) <- fixed.vars
  } else {
    int.loc <- grep(":", fixed.vars)
    colnames(Xmat) <- fixed.vars[-int.loc]
  } 
  Xmat <- model.matrix(fixed, data.frame(Xmat))
  Xmat
}

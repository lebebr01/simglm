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

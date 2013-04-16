#' Master function to simulate single level data.
#' 
#' Takes simulation parameters as inputs and returns simulated data.
#' 
#' Simulates data for the simple regression models.  Returns a data frame with ID variables, 
#' fixed effects, and many other variables useful to help when running simulation studies.
#' 
#' @seealso \code{\link{sim.reg}} for a convenient wrapper for all data conditions.
#' 
#' @param fixed One sided formula for fixed effects in the simulation.  To suppress intercept add -1 to formula.
#' @param fixed.param Fixed effect parameter values (i.e. beta weights).  Must be same length as fixed.
#' @param cov.param List of mean and standard deviation for fixed effects. Does not include intercept, time, or 
#' interactions. Must be same order as fixed formula above.
#' @param n Cluster sample size.
#' @param errorVar Scalar of error variance.
#' @param err.dist Simulated within cluster error distribution. Must be "lap", "chi", "norm", "bimod", 
#' "norm" is default.
#' @param data.str Type of data. Must be "cross", "long", or "single".
#' @param num.dist Number of distributions for bimodal random variables.
#' @export 
sim.reg.single <- function(fixed, fixed.param, cov.param, n, errorVar, err.dist, data.str, num.dist) {
  
  require(MASS)
  require(Matrix)
  
  fixed.vars <- attr(terms(fixed),"term.labels")    ##Extracting fixed effect term labels

  if({length(fixed.vars)+1} != {length(fixed.param)}) stop("Fixed lengths not equal")
  
  Xmat <- fixef.sim.single(fixed, fixed.vars, n, cov.param)
  
  err <- err.sim.single(errorVar, n, err.dist, num.dist)
  
  sim.data <- data.reg.single(Xmat, fixed.param, n, err)
  
  Xmat <- data.frame(Xmat,sim.data)
  Xmat$ID <- 1:n
  Xmat
}

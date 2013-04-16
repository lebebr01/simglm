#' Function to simulate power.
#' 
#' Input simulation conditions and which term to compute power for, export reported power.
#' 
#' Power function to compute power for a regression term for simple regression models.  This
#' function would need to be replicated to make any statement about power.  Use \code{\link{sim.pow}}
#' as a convenient wrapper for this.
#' 
#' @seealso \code{\link{sim.pow}} for a wrapper to replicate.
#' 
#' @param fixed One sided formula for fixed effects in the simulation.  To suppress intercept add -1 to formula.
#' @param fixed.param Fixed effect parameter values (i.e. beta weights).  Must be same length as fixed.
#' @param cov.param List of mean and standard deviation for fixed effects. Does not include intercept, time, or 
#' interactions. Must be same order as fixed formula above.
#' @param n Cluster sample size.
#' @param errorVar Scalar of error variance.
#' @param err.dist Simulated within cluster error distribution. Must be "lap", "chi", "norm", "bimod", 
#' "norm" is default.
#' @param pow.param Number of parameter to calculate power includes intercept where applicable.
#' @param alpha What should the per test alpha rate be used for the hypothesis testing.
#' @param pow.dist Which distribution should be used when testing hypothesis test, z or t?
#' @param pow.tail One-tailed or two-tailed test?
#' @export 
sim.pow.single <- function(fixed, fixed.param, cov.param, n, errorVar, err.dist, 
                           pow.param, alpha, pow.dist = c("z", "t"), pow.tail = c(1, 2)){
  
  temp.single <- sim.reg.single(fixed, fixed.param, cov.param, n, errorVar, err.dist)
  fixed.vars <- attr(terms(fixed),"term.labels")
  
  fm1 <- as.formula(paste("sim.data ~", paste(fixed.vars, collapse = "+")))
  
  temp.lm <- lm(fm1, data = temp.single)
  
  crit <- ifelse(pow.dist == "z", qnorm(alpha/pow.tail, lower.tail = FALSE), 
                 qt(alpha/pow.tail, df = nrow(temp.single) - length(fixed.param), lower.tail = FALSE))
  testStat <- ifelse(pow.tail == 2, abs(coefficients(summary(temp.lm))[pow.param, 3]), 
                 coefficients(summary(temp.lm))[pow.param, 3])
  
  reject <- ifelse(tval >= crit, 1, 0)
  
  return(reject)
}

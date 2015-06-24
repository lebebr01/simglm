#' Master power simulation function.
#' 
#' Input simulation conditions, returns power for term.
#' 
#' This function is a wrapper that replicates the simulation functions for simple regression
#' and the linear mixed model power functions.  This function replicates the power call a 
#' specified number of times and prints outs a matrix with the results.
#' 
#' @param fixed One sided formula for fixed effects in the simulation.  To suppress intercept add -1 to formula.
#' @param random One sided formula for random effects in the simulation. Must be a subset of fixed.
#' @param fixed.param Fixed effect parameter values (i.e. beta weights).  Must be same length as fixed.
#' @param random.param Variance of random effects. Must be same length as random.
#' @param w.var Number of within cluster variables, including intercept if applicable.   
#' Also could be number of level one covariates for cross-sectional clustering.
#' @param cov.param List of mean and variance for fixed effects. Does not include intercept, time, or 
#' interactions. Must be same order as fixed formula above.
#' @param n Cluster sample size.
#' @param p Within cluster sample size.
#' @param errorVar Scalar of error variance.
#' @param randCor Correlation between random effects.
#' @param rand.dist Simulated random effect distribution.  Must be "lap", "chi", "norm", "bimod", 
#' "norm" is default.
#' @param err.dist Simulated within cluster error distribution. Must be "lap", "chi", "norm", "bimod", 
#' "norm" is default.
#' @param serCor Simulation of serial correlation. Must be "AR", "MA", "ARMA", or "ID", "ID" is default.
#' @param serCorVal Serial correlation parameters. A list of values to pass on to arima.sim.
#' @param data.str Type of data. Must be "cross", "long", or "single".
#' @param pow.param Number of parameter to calculate power includes intercept where applicable.
#' @param alpha What should the per test alpha rate be used for the hypothesis testing.
#' @param pow.dist Which distribution should be used when testing hypothesis test, z or t?
#' @param pow.tail One-tailed or two-tailed test?
#' @param replicates How many replications should be done (i.e. the denominator in power calculation).
#' @param parallel Whether the computation should be done in parallel, 
#'  must have parallel backend registered first.  Currently not supported.
#' @export 
sim.pow <- function(fixed, random, fixed.param, random.param, w.var, cov.param, n, p, errorVar, randCor, 
                    rand.dist, err.dist, serCor, serCorVal, data.str, pow.param, alpha, 
                    pow.dist = c("z", "t"), pow.tail = c(1, 2), replicates, parallel){
  
  if(data.str == "single"){
    temp.pow <- replicate(replicates, sim.pow.single(fixed, fixed.param, cov.param, n, errorVar,
                                                     err.dist, pow.param, alpha, pow.dist, pow.tail))
    
    #nbatch <- 3
    #system.time(temp.pow <- foreach(i = 1:(9999/nbatch), .combine = "c", .packages = "simReg") %dopar% {
     # replicate(nbatch, sim.pow.single(fixed, fixed.param, cov.param, n, errorVar,
      #               err.dist, pow.param, alpha, pow.dist, pow.tail))
    #})
  } else {
    temp.pow <- replicate(replicates, sim.pow.nested(fixed, random, fixed.param, random.param, w.var, 
                                                     cov.param, n, p, errorVar, randCor, rand.dist, err.dist, 
                                                     serCor, serCorVal, data.str, pow.param, alpha, 
                                                     pow.dist, pow.tail))
  }
  
  powerMat <- data.frame(cbind(matrix(table(temp.pow), ncol = 2, nrow = 1), table(temp.pow)[2]/length(temp.pow)))
  names(powerMat) <- c("Not Rejected", "Rejected", "Power Prop")
  
  powerMat
  
}

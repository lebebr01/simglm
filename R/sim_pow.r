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
#' @param random3 One sided formula for random effects at third level in the simulation. Must be a subset of fixed
#'  (and likely of random).
#' @param fixed.param Fixed effect parameter values (i.e. beta weights).  Must be same length as fixed.
#' @param random.param Variance of random effects. Must be same length as random.
#' @param random.param3 Variance of third level random effects. Must be same length as random3.
#' @param cov.param List of mean and variance for fixed effects. Does not include intercept, time, or 
#' interactions. Must be same order as fixed formula above.
#' @param k Number of third level clusters.
#' @param n Cluster sample size.
#' @param p Within cluster sample size.
#' @param error_var Scalar of error variance.
#' @param randCor Correlation between random effects.
#' @param randCor3 Correlation between third level random effects.
#' @param rand_dist Simulated random effect distribution.  Must be "lap", "chi", "norm", "bimod", 
#' "norm" is default.
#' @param rand_gen Distribution function to pass on to the level one
#'                  simulation of errors.
#' @param arima TRUE/FALSE flag indicating whether residuals should 
#'             be correlated. If TRUE, must specify a valid model to pass to 
#'             arima.sim. See \code{\link{arima.sim}} for examples.
#' @param data_str Type of data. Must be "cross", "long", or "single".
#' @param fact.vars A nested list of factor, categorical, or ordinal variable specification, 
#'      each list must include numlevels and var.type (must be "lvl1" or "lvl2");
#'      optional specifications are: replace, prob, value.labels.
#' @param unbal A vector of sample sizes for the number of observations for each level 2
#'  cluster. Must have same length as level two sample size n. Alternative specification
#'  can be TRUE, which uses additional argument, unbalCont.
#' @param unbal3 A vector of sample sizes for the number of observations for each level 3
#'  cluster. Must have same length as level two sample size k. Alternative specification
#'  can be TRUE, which uses additional argument, unbalCont3.
#' @param unbalCont When unbal = TRUE, this specifies the minimum and maximum level one size,
#'  will be drawn from a random uniform distribution with min and max specified.
#' @param unbalCont3 When unbal3 = TRUE, this specifies the minimum and maximum level two size,
#'  will be drawn from a random uniform distribution with min and max specified.
#'  @param ... Additional parameters passed on to the level one error generating function
#' @param pow_param Number of parameter to calculate power includes intercept where applicable.
#' @param alpha What should the per test alpha rate be used for the hypothesis testing.
#' @param pow_dist Which distribution should be used when testing hypothesis test, z or t?
#' @param pow_tail One-tailed or two-tailed test?
#' @param replicates How many replications should be done (i.e. the denominator in power calculation).
#' @export 
sim_pow <- function(fixed, random, random3, fixed.param, random.param, random.param3, cov.param, k, n, p, 
                    error_var, randCor, randCor3, rand_dist, rand_gen, arima = FALSE,
                    data_str, fact.vars = list(NULL), unbal = FALSE, unbal3 = FALSE, 
                    unbalCont = NULL, unbalCont3 = NULL,
                    ..., pow_param, alpha, 
                    pow_dist = c("z", "t"), pow_tail = c(1, 2), replicates){
  
  if(data_str == "single"){
    temp.pow <- replicate(replicates, sim_pow_single(fixed, fixed.param, cov.param, n, error_var,
                                                     rand_gen, arima, pow_param, alpha, pow_dist, pow_tail))
    
    #nbatch <- 3
    #system.time(temp.pow <- foreach(i = 1:(9999/nbatch), .combine = "c", .packages = "simReg") %dopar% {
     # replicate(nbatch, sim.pow.single(fixed, fixed.param, cov.param, n, error_var,
      #               rand_gen, pow_param, alpha, pow_dist, pow_tail))
    #})
  } else {
    temp.pow <- replicate(replicates, sim_pow_nested(fixed, random, fixed.param, random.param, cov.param, n, p, 
                                                     error_var, randCor, rand_dist, rand_gen, arima,
                                                     data_str, fact.vars, unbal, unbalCont, ..., pow_param, alpha, 
                                                     pow_dist, pow_tail))
  }
  
  powerMat <- data.frame(cbind(matrix(table(temp.pow), ncol = 2, nrow = 1), table(temp.pow)[2]/length(temp.pow)))
  names(powerMat) <- c("Not Rejected", "Rejected", "Power Prop")
  
  powerMat
  
}

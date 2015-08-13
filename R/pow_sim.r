#' Power simulation for nested designs
#' 
#' Takes simulation conditions as input, exports power.
#' 
#' Power function to compute power for a regression term for the linear mixed model.  This
#' function would need to be replicated to make any statement about power.  Use \code{\link{sim_pow}}
#' as a convenient wrapper for this.
#' 
#' @seealso \code{\link{sim_pow}} for a wrapper to replicate.
#' 
#' @param fixed One sided formula for fixed effects in the simulation.  To suppress intercept add -1 to formula.
#' @param random One sided formula for random effects in the simulation. Must be a subset of fixed.
#' @param fixed.param Fixed effect parameter values (i.e. beta weights).  Must be same length as fixed.
#' @param random.param Variance of random effects. Must be same length as random.
#' @param cov.param List of mean, sd (standard deviations), and var.type for fixed effects. 
#'  Does not include intercept, time, factors, or interactions. 
#'  var.type must be either "lvl1" or "lvl2". Must be same order as fixed formula above.
#' @param n Cluster sample size.
#' @param p Within cluster sample size.
#' @param error_var Scalar of error variance.
#' @param randCor Correlation between random effects.
#' @param rand_dist Simulated random effect distribution.  Must be "lap", "chi", "norm", "bimod", 
#' "norm" is default.
#' @param rand_gen Simulated within cluster error distribution. Must be "lap", "chi", "norm", "bimod", 
#' "norm" is default.
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
#' @param unbalCont When unbal = TRUE, this specifies the minimum and maximum level one size,
#'  will be drawn from a random uniform distribution with min and max specified.
#' @param ... Additional specification needed to pass to the random generating 
#'             function defined by rand.gen.
#' @param pow_param Number of parameter to calculate power includes intercept where applicable.
#' @param alpha What should the per test alpha rate be used for the hypothesis testing.
#' @param pow_dist Which distribution should be used when testing hypothesis test, z or t?
#' @param pow_tail One-tailed or two-tailed test?
#' @export 
sim_pow_nested <- function(fixed, random, fixed.param, random.param, cov.param, n, p, 
                           error_var, randCor, rand_dist, rand_gen, arima,
                           data_str, fact.vars, unbal, unbalCont, ...,
                           pow_param, alpha, pow_dist = c("z", "t"), pow_tail = c(1, 2)){

  temp.nest <- sim_reg_nested(fixed, random, fixed.param, random.param, cov.param, n, p, 
                              error_var, randCor, rand_dist, rand_gen, arima,
                              data_str, fact.vars, unbal, unbalCont, ...)
  
  fixed.vars <- attr(terms(fixed),"term.labels")    ##Extracting fixed effect term labels
  rand.vars <- attr(terms(random),"term.labels")
  
  fix1 <- paste("sim.data ~", paste(fixed.vars, collapse = "+"))
  #ran1 <- paste("(", paste(rand.vars, collapse = "+"), "|clustID)", sep = "")
  #fm1 <- as.formula(paste(fix1, ran1, sep = "+ "))
  ran1 <- paste("~", paste(rand.vars, collapse = "+"), "|clustID", sep = "")
  
  #temp.lme <- lme(fixed = as.formula(fix1), data = temp.nest, random = as.formula(ran1))
  
#   crit <- qnorm(alpha/pow_tail, lower.tail = FALSE)
#   #coefTab <- coef.tbl(temp.lmer)
#   #testStat <- coefTab[pow_param]
#   #testStat <- summary(temp.lme)$tTable[pow_param, 3]
#   
#   reject <- ifelse(testStat >= crit, 1, 0)
#   
#   return(reject)  
}


#' Function to simulate power.
#' 
#' Input simulation conditions and which term to compute power for, export reported power.
#' 
#' Power function to compute power for a regression term for simple regression models.  This
#' function would need to be replicated to make any statement about power.  Use \code{\link{sim_pow}}
#' as a convenient wrapper for this.
#' 
#' @seealso \code{\link{sim_pow}} for a wrapper to replicate.
#' 
#' @param fixed One sided formula for fixed effects in the simulation.  To suppress intercept add -1 to formula.
#' @param fixed.param Fixed effect parameter values (i.e. beta weights).  Must be same length as fixed.
#' @param cov.param List of mean and standard deviation for fixed effects. Does not include intercept, time, or 
#' interactions. Must be same order as fixed formula above.
#' @param n Cluster sample size.
#' @param error_var Scalar of error variance.
#' @param rand_gen Simulated within cluster error distribution. Must be "lap", "chi", "norm", "bimod", 
#' "norm" is default.
#' @param pow_param Number of parameter to calculate power includes intercept where applicable.
#' @param alpha What should the per test alpha rate be used for the hypothesis testing.
#' @param pow_dist Which distribution should be used when testing hypothesis test, z or t?
#' @param pow_tail One-tailed or two-tailed test?
#' @export 
sim_pow_single <- function(fixed, fixed.param, cov.param, n, error_var, rand_gen, 
                           pow_param, alpha, pow_dist = c("z", "t"), pow_tail = c(1, 2)){
  
  temp.single <- sim_reg_single(fixed, fixed.param, cov.param, n, 
                                error_var, rand_gen)
  fixed.vars <- attr(terms(fixed),"term.labels")
  
  fm1 <- as.formula(paste("sim.data ~", paste(fixed.vars, collapse = "+")))
  
  temp.lm <- lm(fm1, data = temp.single)
  
  crit <- ifelse(pow_dist == "z", qnorm(alpha/pow_tail, lower.tail = FALSE), 
                 qt(alpha/pow_tail, df = nrow(temp.single) - length(fixed.param), lower.tail = FALSE))
  testStat <- ifelse(pow_tail == 2, abs(coefficients(summary(temp.lm))[pow_param, 3]), 
                     coefficients(summary(temp.lm))[pow_param, 3])
  
  reject <- ifelse(testStat >= crit, 1, 0)
  
  return(reject)
}

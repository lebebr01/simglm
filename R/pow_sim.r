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
#' @param fixed_param Fixed effect parameter values (i.e. beta weights).  Must be same length as fixed.
#' @param random.param Variance of random effects. Must be same length as random.
#' @param cov_param List of mean, sd (standard deviations), and var.type for fixed effects. 
#'  Does not include intercept, time, factors, or interactions. 
#'  var.type must be either "lvl1" or "lvl2". Must be same order as fixed formula above.
#' @param n Cluster sample size.
#' @param p Within cluster sample size.
#' @param error_var Scalar of error variance.
#' @param randCor Correlation between random effects.
#' @param rand_dist Simulated random effect distribution.  Must be "lap", "chi", "norm", "bimod", 
#' "norm" is default.
#' @param with_err_gen Simulated within cluster error distribution. Must be "lap", "chi", "norm", "bimod", 
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
sim_pow_nested <- function(fixed, random, fixed_param, random.param, cov_param, n, p, 
                           error_var, randCor, rand_dist, with_err_gen, arima,
                           data_str, fact.vars, unbal, unbalCont, ...,
                           pow_param, alpha, pow_dist = c("z", "t"), pow_tail = c(1, 2)){

  temp.nest <- sim_reg_nested(fixed, random, fixed_param, random.param, cov_param, n, p, 
                              error_var, randCor, rand_dist, with_err_gen, arima,
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
#' @param fixed_param Fixed effect parameter values (i.e. beta weights).  Must be same length as fixed.
#' @param cov_param List of mean and sd (standard deviation) for fixed effects. Does not include intercept, time, or 
#'   interactions. Must be same order as fixed formula above.
#' @param n Cluster sample size.
#' @param error_var Scalar of error variance.
#' @param with_err_gen Simulated within cluster error distribution. Must be a quoted 'r' distribution
#'               function.
#' @param arima TRUE/FALSE flag indicating whether residuals should 
#'               be correlated. If TRUE, must specify a valid model to pass to 
#'               arima.sim. See \code{\link{arima.sim}} for examples.
#' @param data_str Type of data. Must be "cross", "long", or "single".
#' @param cor_vars A vector of correlations between variables.
#' @param fact_vars A nested list of factor, categorical, or ordinal variable specification, 
#'      each list must include numlevels and var_type (must be "lvl1" or "lvl2");
#'      optional specifications are: replace, prob, value.labels.
#' @param pow_param Name of variable to calculate power for, must be a name from fixed.
#' @param alpha What should the per test alpha rate be used for the hypothesis testing.
#' @param pow_dist Which distribution should be used when testing hypothesis test, z or t?
#' @param pow_tail One-tailed or two-tailed test?
#' @param ... Additional specification needed to pass to the random generating 
#'             function defined by with_err_gen.
#' @export 
sim_pow_single <- function(fixed, fixed_param, cov_param, n, error_var, with_err_gen,
                           arima = FALSE, data_str, cor_vars = NULL, fact_vars = list(NULL), 
                           pow_param = NULL, alpha, pow_dist = c("z", "t"), pow_tail = c(1, 2), ...) {
  
  fixed_vars <- attr(terms(fixed),"term.labels")
  
  if(any(pow_param %ni% fixed_vars)) { stop('pow_param must be a subset of ')}
  
  temp_single <- sim_reg_single(fixed, fixed_param, cov_param, n, error_var, with_err_gen, 
                                arima, data_str, 
                                cor_vars, fact_vars, ...)
  fm1 <- as.formula(paste("sim_data ~", paste(fixed_vars, collapse = "+")))
  
  temp_lm <- lm(fm1, data = temp_single)
  
  crit <- ifelse(pow_dist == "z", qnorm(alpha/pow_tail, lower.tail = FALSE), 
                 qt(alpha/pow_tail, df = nrow(temp_single) - length(fixed_param), lower.tail = FALSE))
  test_stat <- data.frame(abs(coefficients(summary(temp_lm))[, 3]))
  if(is.null(pow_param) == FALSE) {
    test_stat <- test_stat[pow_param, ]
  }
  
  reject <- data.frame(var = pow_param,
                       test_stat = test_stat)
  reject$reject <- ifelse(test_stat >= crit, 1, 0)
  
  return(reject)
}

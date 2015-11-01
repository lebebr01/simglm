#' Master continuous simulation function.
#' 
#' Takes simulation parameters as inputs and returns simulated data.
#' 
#' Simulated data is useful for classroom demonstrations and to study 
#' the impacts of assumption violations on parameter estimates, statistical
#' power, or empirical type I error rates.
#' 
#' This function allows researchers a flexible approach to simulate regression
#' models, including single level models and cross sectional or longitudinal
#' linear mixed models (aka. hierarchical linear models or multilevel models).
#' 
#' @param fixed One sided formula for fixed effects in the simulation.  To suppress intercept add -1 to formula.
#' @param random One sided formula for random effects in the simulation. Must be a subset of fixed.
#' @param random3 One sided formula for random effects at third level in the simulation. Must be a subset of fixed
#'  (and likely of random).
#' @param fixed.param Fixed effect parameter values (i.e. beta weights).  Must be same length as fixed.
#' @param random_param A list of named elements that must contain: 
#'             random.param = variance of random parameters,
#'             rand_gen = Name of simulation function for random effects.
#'          Optional elements are:
#'             ther: Theorectial mean and variance from rand_gen,
#'             ther_sim: Simulate mean/variance for standardization purposes,
#'             cor_vars: Correlation between random effects,
#'             ...: Additional parameters needed for rand_gen function.
#' @param random_param3 A list of named elements that must contain: 
#'             random.param = variance of random parameters,
#'             rand_gen = Name of simulation function for random effects.
#'          Optional elements are:
#'             ther: Theorectial mean and variance from rand_gen,
#'             ther_sim: Simulate mean/variance for standardization purposes,
#'             cor_vars: Correlation between random effects,
#'             ...: Additional parameters needed for rand_gen function.
#' @param cov.param List of mean and variance for fixed effects. Does not include intercept, time, or 
#' interactions. Must be same order as fixed formula above.
#' @param k Number of third level clusters.
#' @param n Cluster sample size.
#' @param p Within cluster sample size.
#' @param error_var Scalar of error variance.
#' @param with_err_gen Distribution function to pass on to the level one
#'                  simulation of errors.
#' @param arima TRUE/FALSE flag indicating whether residuals should 
#'             be correlated. If TRUE, must specify a valid model to pass to 
#'             arima.sim. See \code{\link{arima.sim}} for examples.
#' @param data_str Type of data. Must be "cross", "long", or "single".
#' @param cor_vars A vector of correlations between variables.
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
#' @export 
#' @examples
#' \donttest{
#' # generating parameters for single level regression
#' fixed <- ~1 + act + diff + numCourse + act:numCourse
#' fixed.param <- c(2, 4, 1, 3.5, 2)
#' cov.param <- list(mean = c(0, 0, 0), sd = c(4, 3, 3), var.type = c("single", "single", "single"))
#' n <- 150
#' error_var <- 3
#' with_err_gen <- 'rnorm'
#' temp.single <- sim_reg(fixed = fixed, fixed.param = fixed.param, cov.param = cov.param, 
#' n = n, error_var = error_var, with_err_gen = with_err_gen, data_str = "single")
#' # Fitting regression to obtain parameter estimates
#' summary(lm(sim.data ~ 1 + act + diff + numCourse + act:numCourse, data = temp.single))
#' 
#' # Longitudinal linear mixed model example
#' fixed <- ~1 + time + diff + act + time:act
#' random <- ~1 + time + diff
#' fixed.param <- c(4, 2, 6, 2.3, 7)
#' random_param <- list(random.param = c(7, 4, 2), rand_gen = 'rnorm')
#' cov.param <- list(mean = c(0, 0), sd = c(1.5, 4), var.type = c("lvl1", "lvl2"))
#' n <- 150
#' p <- 30
#' error_var <- 4
#' with_err_gen <- 'rnorm'
#' data_str <- "long"
#' temp.long <- sim_reg(fixed, random, random3 = NULL, fixed.param, random_param, random_param3 = NULL,
#'  cov.param, k = NULL, n, p, error_var, with_err_gen, data_str = data_str)
#' 
#' ## fitting lmer model
#' library(lme4)
#' lmer(sim.data ~ 1 + time + diff + act + time:act + (1 + time + diff | clustID), 
#' data = temp.long)
#' 
#' # Three level example
#' fixed <- ~1 + time + diff + act + actClust + time:act
#' random <- ~1 + time + diff
#' random3 <- ~ 1 + time
#' fixed.param <- c(4, 2, 6, 2.3, 7, 0)
#' random_param <- list(random.param = c(7, 4, 2), rand_gen = 'rnorm')
#' random_param3 <- list(random.param = c(4, 2), rand_gen = 'rnorm')
#' cov.param <- list(mean = c(0, 0, 0), sd = c(1.5, 4, 2), 
#' var.type = c("lvl1", "lvl2", "lvl3"))
#' k <- 10
#' n <- 150
#' p <- 30
#' error_var <- 4
#' with_err_gen <- 'rnorm'
#' data_str <- "long"
#' temp.three <- sim_reg(fixed, random, random3, fixed.param, random_param, 
#' random_param3, cov.param, k,n, p, error_var, with_err_gen, data_str = data_str)
#' 
#' library(lme4)
#' lmer(sim.data ~ 1 + time + diff + act + actClust + time:act + (1 + time + diff | clustID) +  
#' (1 | clust3ID), data = temp.three)
#' 
#' }
sim_reg <- function(fixed, random, random3, fixed.param, 
                    random_param = list(), random_param3 = list(), cov.param, k, n, p, 
                    error_var, with_err_gen, arima = FALSE,
                    data_str, cor_vars = NULL, fact.vars = list(NULL), unbal = FALSE, unbal3 = FALSE, 
                    unbalCont = NULL, unbalCont3 = NULL,
                    ...) {
  
  if(data_str == "single"){
    sim_reg_single(fixed, fixed.param, cov.param, n, error_var, with_err_gen, arima, data_str, 
                   cor_vars, fact.vars, ...)
  } else {
  	if (is.null(k)){
  	  sim_reg_nested(fixed, random, fixed.param, random_param, cov.param, n, p, 
  	                 error_var, with_err_gen, arima,
  	                 data_str, cor_vars, fact.vars, unbal, unbalCont, ...)
  } else {
    sim_reg_nested3(fixed, random, random3, fixed.param, random_param, random_param3, 
                    cov.param, k, n, p, 
                    error_var, with_err_gen, arima,
                    data_str, cor_vars, fact.vars, unbal, unbal3, unbalCont, unbalCont3, ...)
  }
 }
}

#' Master generalized simulation function.
#' 
#' Takes simulation parameters as inputs and returns simulated data.
#' 
#' Simulated data is useful for classroom demonstrations and to study 
#' the impacts of assumption violations on parameter estimates, statistical
#' power, or empirical type I error rates.
#' 
#' This function allows researchers a flexible approach to simulate regression
#' models, including single level models and cross sectional or longitudinal
#' linear mixed models (aka. hierarchical linear models or multilevel models).
#' 
#' @param fixed One sided formula for fixed effects in the simulation.  To suppress intercept add -1 to formula.
#' @param random One sided formula for random effects in the simulation. Must be a subset of fixed.
#' @param random3 One sided formula for random effects at third level in the simulation. Must be a subset of fixed
#'  (and likely of random).
#' @param fixed.param Fixed effect parameter values (i.e. beta weights).  Must be same length as fixed.
#' @param random_param A list of named elements that must contain: 
#'             random.param = variance of random parameters,
#'             rand_gen = Name of simulation function for random effects.
#'          Optional elements are:
#'             ther: Theorectial mean and variance from rand_gen,
#'             ther_sim: Simulate mean/variance for standardization purposes,
#'             cor_vars: Correlation between random effects,
#'             ...: Additional parameters needed for rand_gen function.
#' @param random_param3 A list of named elements that must contain: 
#'             random.param = variance of random parameters,
#'             rand_gen = Name of simulation function for random effects.
#'          Optional elements are:
#'             ther: Theorectial mean and variance from rand_gen,
#'             ther_sim: Simulate mean/variance for standardization purposes,
#'             cor_vars: Correlation between random effects,
#'             ...: Additional parameters needed for rand_gen function.
#' @param cov.param List of mean and variance for fixed effects. Does not include intercept, time, or 
#' interactions. Must be same order as fixed formula above.
#' @param k Number of third level clusters.
#' @param n Cluster sample size.
#' @param p Within cluster sample size.
#' @param data_str Type of data. Must be "cross", "long", or "single".
#' @param cor_vars A vector of correlations between variables.
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
#' @export 
#' 
#' @examples
#' \donttest{
#' # generating parameters for single level regression
#' fixed <- ~1 + act + diff + numCourse + act:numCourse
#' fixed.param <- c(2, 4, 1, 3.5, 2)
#' cov.param <- list(mean = c(0, 0, 0), sd = c(4, 3, 3), var.type = c("single", "single", "single"))
#' n <- 150
#' temp.single <- sim_glm(fixed = fixed, fixed.param = fixed.param, cov.param = cov.param, 
#' n = n, data_str = "single")
#' 
#' # Longitudinal linear mixed model example
#' fixed <- ~1 + time + diff + act + time:act
#' random <- ~1 + time + diff
#' fixed.param <- c(4, 2, 6, 2.3, 7)
#' random_param <- list(random.param = c(7, 4, 2), rand_gen = 'rnorm')
#' cov.param <- list(mean = c(0, 0), sd = c(1.5, 4), var.type = c("lvl1", "lvl2"))
#' n <- 150
#' p <- 30
#' data_str <- "long"
#' temp.long <- sim_glm(fixed, random, random3 = NULL, fixed.param, random_param, random_param3 = NULL,
#'  cov.param, k = NULL, n, p, data_str = data_str)
#' 
#' 
#' # Three level example
#' fixed <- ~1 + time + diff + act + actClust + time:act
#' random <- ~1 + time + diff
#' random3 <- ~ 1 + time
#' fixed.param <- c(4, 2, 6, 2.3, 7, 0)
#' random_param <- list(random.param = c(7, 4, 2), rand_gen = 'rnorm')
#' random_param3 <- list(random.param = c(4, 2), rand_gen = 'rnorm')
#' cov.param <- list(mean = c(0, 0, 0), sd = c(1.5, 4, 2), 
#' var.type = c("lvl1", "lvl2", "lvl3"))
#' k <- 10
#' n <- 150
#' p <- 30
#' data_str <- "long"
#' temp.three <- sim_glm(fixed, random, random3, fixed.param, random_param, 
#' random_param3, cov.param, k,n, p, data_str = data_str)
#' 
#' 
#' }
sim_glm <- function(fixed, random, random3, fixed.param, random_param = list(), 
                    random_param3 = list(), cov.param, k, n, p, 
                    data_str, cor_vars = NULL, fact.vars = list(NULL), 
                    unbal = FALSE, unbal3 = FALSE, 
                    unbalCont = NULL, unbalCont3 = NULL) {
  
  if(data_str == "single"){
    sim_glm_single(fixed, fixed.param, cov.param, n, data_str, cor_vars, fact.vars)
  } else {
    if (is.null(k)){
      sim_glm_nested(fixed, random, fixed.param, random_param, cov.param, n, p, 
                     data_str, cor_vars, fact.vars, unbal, unbalCont)
    } else {
      sim_glm_nested3(fixed, random, random3, fixed.param, random_param, random_param3,
                      cov.param, k, n, p, 
                      data_str, cor_vars, fact.vars, unbal, unbal3, unbalCont, unbalCont3)
    }
  }
}



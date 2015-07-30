#' Master simulation function.
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
#' @param rand.dist Simulated random effect distribution.  Must be "lap", "chi", "norm", "bimod", 
#' "norm" is default.
#' @param rand_gen Distribution function to pass on to the level one
#'                  simulation of errors.
#' @param arima TRUE/FALSE flag indicating whether residuals should 
#'             be correlated. If TRUE, must specify a valid model to pass to 
#'             arima.sim. See \code{\link{arima.sim}} for examples.
#' @param data.str Type of data. Must be "cross", "long", or "single".
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
#' rand_gen <- rnorm
#' temp.single <- sim_reg(fixed = fixed, fixed.param = fixed.param, cov.param = cov.param, 
#' n = n, error_var = error_var, rand_gen = rand_gen, data.str = "single")
#' # Fitting regression to obtain parameter estimates
#' summary(lm(sim.data ~ 1 + act + diff + numCourse + act:numCourse, data = temp.single))
#' 
#' # Longitudinal linear mixed model example
#' fixed <- ~1 + time + diff + act + time:act
#' random <- ~1 + time + diff
#' fixed.param <- c(4, 2, 6, 2.3, 7)
#' random.param <- c(7, 4, 2)
#' cov.param <- list(mean = c(0, 0), sd = c(1.5, 4), var.type = c("lvl1", "lvl2"))
#' n <- 150
#' p <- 30
#' error_var <- 4
#' randCor <- 0
#' rand.dist <- "norm"
#' rand_gen <- rnorm
#' data.str <- "long"
#' temp.long <- sim_reg(fixed, random, random3 = NULL, fixed.param, random.param, random.param3 = NULL,
#'  cov.param, k = NULL, n, p, error_var, randCor, randCor3 = NULL, rand.dist, rand_gen, 
#'  data.str)
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
#' random.param <- c(7, 4, 2)
#' random.param3 <- c(4, 2)
#' cov.param <- list(mean = c(0, 0, 0), sd = c(1.5, 4, 2), 
#' var.type = c("lvl1", "lvl2", "lvl3"))
#' k <- 10
#' n <- 150
#' p <- 30
#' error_var <- 4
#' randCor <- 0
#' randCor3 <- 0
#' rand.dist <- "norm"
#' rand_gen <- rnorm
#' data.str <- "long"
#' temp.three <- sim_reg(fixed, random, random3, fixed.param, random.param, 
#' random.param3, cov.param, k,n, p, error_var, randCor, randCor3, rand.dist,
#' rand_gen, data.str)
#' 
#' library(lme4)
#' lmer(sim.data ~ 1 + time + diff + act + actClust + time:act + (1 + time + diff | clustID) +  
#' (1 | clust3ID), data = temp.three)
#' 
#' }
sim_reg <- function(fixed, random, random3, fixed.param, random.param, random.param3, cov.param, k, n, p, 
                    error_var, randCor, randCor3, rand.dist, rand_gen, arima = FALSE,
                    data.str, fact.vars = list(NULL), unbal = FALSE, unbal3 = FALSE, 
                    unbalCont = NULL, unbalCont3 = NULL,
                    ...) {
  
  if(data.str == "single"){
    sim_reg_single(fixed, fixed.param, cov.param, n, error_var, rand_gen, arima, data.str, fact.vars, ...)
  } else {
  	if (is.null(k)){
  	  sim_reg_nested(fixed, random, fixed.param, random.param, cov.param, n, p, 
  	                 error_var, randCor, rand.dist, rand_gen, arima,
  	                 data.str, fact.vars, unbal, unbalCont, ...)
  } else {
    sim_reg_nested3(fixed, random, random3, fixed.param, random.param, random.param3, cov.param, k, n, p, 
                    error_var, randCor, randCor3, rand.dist, rand_gen, arima,
                    data.str, fact.vars, unbal, unbal3, unbalCont, unbalCont3, ...)
  }
 }
}

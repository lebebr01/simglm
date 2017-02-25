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
#' @param fixed One sided formula for fixed effects in the simulation.  
#'   To suppress intercept add -1 to formula.
#' @param random One sided formula for random effects in the simulation. 
#'   Must be a subset of fixed.
#' @param random3 One sided formula for random effects at third level in the 
#'   simulation. Must be a subset of fixed (and likely of random).
#' @param fixed_param Fixed effect parameter values (i.e. beta weights).  
#'   Must be same length as fixed.
#' @param random_param A list of named elements that must contain: 
#'   \itemize{
#'      \item  random_var = variance of random parameters,
#'      \item  rand_gen = Name of simulation function for random effects.
#'   }
#'          Optional elements are:
#'   \itemize{
#'      \item ther: Theorectial mean and variance from rand_gen,
#'      \item ther_sim: Simulate mean/variance for standardization purposes,
#'      \item cor_vars: Correlation between random effects,
#'      \item ...: Additional parameters needed for rand_gen function.
#'    }
#' @param random_param3 A list of named elements that must contain: 
#'    \itemize{
#'        \item random_var = variance of random parameters,
#'        \item rand_gen = Name of simulation function for random effects.
#'    }
#'          Optional elements are:
#'    \itemize{
#'        \item ther: Theorectial mean and variance from rand_gen,
#'        \item ther_sim: Simulate mean/variance for standardization purposes,
#'        \item cor_vars: Correlation between random effects,
#'        \item ...: Additional parameters needed for rand_gen function.
#'    }
#' @param cov_param List of arguments to pass to the continuous generating 
#'   function. Required arguments include:
#'   \itemize{
#'     \item dist_fun: This is a quoted R distribution function.
#'     \item var_type: This is the level of variable to generate. Must be 
#'       either 'lvl1', 'lvl2', or 'lvl3'. Must be same order as fixed formula 
#'       above.
#'   }
#'   Optional arguments to the distribution functions are in a nested list,
#'    see the examples for example code for this.
#'  Does not include intercept, time, factors, or interactions.
#'    
#' @param k Number of third level clusters.
#' @param n Cluster sample size.
#' @param p Within cluster sample size.
#' @param error_var Scalar of error variance.
#' @param with_err_gen Distribution function to pass on to the level one
#'                  simulation of errors.
#' @param arima TRUE/FALSE flag indicating whether residuals should 
#'             be correlated. If TRUE, must specify a valid model to pass to 
#'             arima.sim via the arima_mod argument. 
#'             See \code{\link{arima.sim}} for examples.
#' @param data_str Type of data. Must be "cross", "long", or "single".
#' @param cor_vars A vector of correlations between variables.
#' @param fact_vars A nested list of factor, categorical, or ordinal variable 
#'      specification, each list must include:
#'   \itemize{
#'        \item numlevels = Number of levels for ordinal or factor variables.
#'        \item var_type = Must be 'single', 'lvl1', 'lvl2', or 'lvl3'.
#'    }
#'    Optional arguments include:
#'    \itemize{
#'        \item replace
#'        \item prob
#'        \item value.labels
#'    }
#'     See also \code{\link{sample}} for use of these optional arguments.
#' @param unbal A vector of sample sizes for the number of observations for 
#'  each level 2 cluster. Must have same length as level two sample size n. 
#'  Alternative specification can be TRUE, which uses additional argument, 
#'  unbalCont.
#' @param unbal3 A vector of sample sizes for the number of observations for 
#'  each level 3 cluster. Must have same length as level two sample size k. 
#'  Alternative specification can be TRUE, which uses additional argument, 
#'  unbalCont3.
#' @param unbalCont When unbal = TRUE, this specifies the minimum and 
#'  maximum level one size, will be drawn from a random uniform distribution 
#'  with min and max specified.
#' @param unbalCont3 When unbal3 = TRUE, this specifies the minimum and 
#'  maximum level two size, will be drawn from a random uniform distribution 
#'  with min and max specified.
#' @param lvl1_err_params Additional parameters passed as a list on to the 
#'  level one error generating function
#' @param arima_mod A list indicating the ARIMA model to pass to arima.sim. 
#'             See \code{\link{arima.sim}} for examples.
#' @param ... Not currently used.
#' @import stats
#' @export 
#' @examples
#' 
#' # generating parameters for single level regression
#' fixed <- ~1 + act + diff + numCourse + act:numCourse
#' fixed_param <- c(2, 4, 1, 3.5, 2)
#' cov_param <- list(dist_fun = c('rnorm', 'rnorm', 'rnorm'), 
#'    var_type = c("single", "single", "single"),
#'    opts = list(list(mean = 0, sd = 4), 
#'    list(mean = 0, sd = 3),
#'    list(mean = 0, sd = 3)))
#' n <- 150
#' error_var <- 3
#' with_err_gen <- 'rnorm'
#' temp_single <- sim_reg(fixed = fixed, fixed_param = fixed_param, 
#'    cov_param = cov_param, 
#'    n = n, error_var = error_var, with_err_gen = with_err_gen, 
#'    data_str = "single")
#' # Fitting regression to obtain parameter estimates
#' summary(lm(sim_data ~ 1 + act + diff + numCourse + act:numCourse, 
#'    data = temp_single))
#' 
#' # Longitudinal linear mixed model example
#' fixed <- ~1 + time + diff + act + time:act
#' random <- ~1 + time + diff
#' fixed_param <- c(4, 2, 6, 2.3, 7)
#' random_param <- list(random_var = c(7, 4, 2), rand_gen = 'rnorm')
#' cov_param <- list(dist_fun = c('rnorm', 'rnorm'), 
#'   var_type = c("lvl1", "lvl2"),
#'   opts = list(list(mean = 0, sd = 1.5), 
#'   list(mean = 0, sd = 4)))
#' n <- 150
#' p <- 30
#' error_var <- 4
#' with_err_gen <- 'rnorm'
#' data_str <- "long"
#' temp_long <- sim_reg(fixed, random, random3 = NULL, fixed_param, 
#'    random_param, random_param3 = NULL,
#'    cov_param, k = NULL, n, p, error_var, with_err_gen, data_str = data_str)
#' 
#' ## fitting lmer model
#' library(lme4)
#' lmer(sim_data ~ 1 + time + diff + act + time:act + 
#'   (1 + time + diff | clustID), 
#'   data = temp_long)
#' 
#' # Three level example
#' fixed <- ~1 + time + diff + act + actClust + time:act
#' random <- ~1 + time + diff
#' random3 <- ~ 1 + time
#' fixed_param <- c(4, 2, 6, 2.3, 7, 0)
#' random_param <- list(random_var = c(7, 4, 2), rand_gen = 'rnorm')
#' random_param3 <- list(random_var = c(4, 2), rand_gen = 'rnorm')
#' cov_param <- list(dist_fun = c('rnorm', 'rnorm', 'rnorm'), 
#'      var_type = c("lvl1", "lvl2", "lvl3"),
#'      opts = list(list(mean = 0, sd = 1.5),
#'      list(mean = 0, sd = 4),
#'      list(mean = 0, sd = 2)))
#' k <- 10
#' n <- 15
#' p <- 10
#' error_var <- 4
#' with_err_gen <- 'rnorm'
#' data_str <- "long"
#' temp_three <- sim_reg(fixed, random, random3, fixed_param, random_param, 
#' random_param3, cov_param, k,n, p, error_var, with_err_gen, 
#'    data_str = data_str)
#' 
#' library(lme4)
#' lmer(sim_data ~ 1 + time + diff + act + actClust + time:act + 
#'    (1 + time + diff | clustID) +  
#'    (1 | clust3ID), data = temp_three)
#' 
sim_reg <- function(fixed, random, random3, fixed_param, 
                    random_param = list(), random_param3 = list(), cov_param, 
                    k, n, p, error_var, with_err_gen, arima = FALSE,
                    data_str, cor_vars = NULL, fact_vars = list(NULL), 
                    unbal = FALSE, unbal3 = FALSE, unbalCont = NULL, 
                    unbalCont3 = NULL, lvl1_err_params = NULL,
                    arima_mod = list(NULL), ...) {
  
  if(data_str == "single"){
    sim_reg_single(fixed, fixed_param, cov_param, n, error_var, with_err_gen, 
                   arima, data_str, cor_vars, fact_vars, lvl1_err_params, 
                   arima_mod, ...)
  } else {
  	if (is.null(k)){
  	  sim_reg_nested(fixed, random, fixed_param, random_param, cov_param, n, p, 
  	                 error_var, with_err_gen, arima, data_str, cor_vars, 
  	                 fact_vars, unbal, unbalCont, lvl1_err_params, 
  	                 arima_mod, ...)
  } else {
    sim_reg_nested3(fixed, random, random3, fixed_param, random_param, 
                    random_param3, cov_param, k, n, p, error_var, with_err_gen, 
                    arima, data_str, cor_vars, fact_vars, unbal, unbal3, 
                    unbalCont, unbalCont3, lvl1_err_params, arima_mod, ...)
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
#' @param fixed One sided formula for fixed effects in the simulation.  
#'   To suppress intercept add -1 to formula.
#' @param random One sided formula for random effects in the simulation. 
#'   Must be a subset of fixed.
#' @param random3 One sided formula for random effects at third level in the 
#'   simulation. Must be a subset of fixed (and likely of random).
#' @param fixed_param Fixed effect parameter values (i.e. beta weights).  
#'   Must be same length as fixed.
#' @param random_param A list of named elements that must contain: 
#'   \itemize{
#'      \item  random_var = variance of random parameters,
#'      \item  rand_gen = Name of simulation function for random effects.
#'   }
#'          Optional elements are:
#'   \itemize{
#'      \item ther: Theorectial mean and variance from rand_gen,
#'      \item ther_sim: Simulate mean/variance for standardization purposes,
#'      \item cor_vars: Correlation between random effects,
#'      \item ...: Additional parameters needed for rand_gen function.
#'    }
#' @param random_param3 A list of named elements that must contain: 
#'    \itemize{
#'        \item random_var = variance of random parameters,
#'        \item rand_gen = Name of simulation function for random effects.
#'    }
#'          Optional elements are:
#'    \itemize{
#'        \item ther: Theorectial mean and variance from rand_gen,
#'        \item ther_sim: Simulate mean/variance for standardization purposes,
#'        \item cor_vars: Correlation between random effects,
#'        \item ...: Additional parameters needed for rand_gen function.
#'    }
#' @param cov_param List of arguments to pass to the continuous generating 
#'   function. Required arguments include:
#'   \itemize{
#'     \item dist_fun: This is a quoted R distribution function.
#'     \item var_type: This is the level of variable to generate. Must be 
#'       either 'lvl1', 'lvl2', or 'lvl3'. Must be same order as fixed formula 
#'       above.
#'   }
#'   Optional arguments to the distribution functions are in a nested list,
#'    see the examples for example code for this.
#'  Does not include intercept, time, factors, or interactions.
#'  
#' @param k Number of third level clusters.
#' @param n Cluster sample size.
#' @param p Within cluster sample size.
#' @param data_str Type of data. Must be "cross", "long", or "single".
#' @param cor_vars A vector of correlations between variables.
#' @param fact_vars A nested list of factor, categorical, or ordinal variable 
#'      specification, each list must include:
#'   \itemize{
#'        \item numlevels = Number of levels for ordinal or factor variables.
#'        \item var_type = Must be 'single', 'lvl1', 'lvl2', or 'lvl3'.
#'    }
#'    Optional arguments include:
#'    \itemize{
#'        \item replace
#'        \item prob
#'        \item value.labels
#'    }
#'     See also \code{\link{sample}} for use of these optional arguments.
#' @param unbal A vector of sample sizes for the number of observations for 
#'  each level 2 cluster. Must have same length as level two sample size n. 
#'  Alternative specification can be TRUE, which uses additional argument, 
#'  unbalCont.
#' @param unbal3 A vector of sample sizes for the number of observations for 
#'  each level 3 cluster. Must have same length as level two sample size k. 
#'  Alternative specification can be TRUE, which uses additional argument, 
#'  unbalCont3.
#' @param unbalCont When unbal = TRUE, this specifies the minimum and maximum 
#'  level one size, will be drawn from a random uniform distribution with min 
#'  and max specified.
#' @param unbalCont3 When unbal3 = TRUE, this specifies the minimum and 
#'  maximum level two size, will be drawn from a random uniform distribution 
#'  with min and max specified.
#' @export 
#' 
#' @examples
#' # generating parameters for single level regression
#' set.seed(2)
#' fixed <- ~1 + act + diff + numCourse + act:numCourse
#' fixed_param <- c(2, 4, 1, 3.5, 2)
#' cov_param <- list(dist_fun = c('rnorm', 'rnorm', 'rnorm'),
#'    var_type = c("single", "single", "single"),
#'    opts = list(list(mean = 0, sd = 4),
#'    list(mean = 0, sd = 3),
#'    list(mean = 0, sd = 3)))
#' n <- 150
#' temp_single <- sim_glm(fixed = fixed, fixed_param = fixed_param, 
#'   cov_param = cov_param, n = n, data_str = "single")
#' 
#' # Longitudinal linear mixed model example
#' fixed <- ~1 + time + diff + act + time:act
#' random <- ~1 + time + diff
#' fixed_param <- c(4, 2, 6, 2.3, 7)
#' random_param <- list(random_var = c(7, 4, 2), rand_gen = 'rnorm')
#' cov_param <- list(dist_fun = c('rnorm', 'rnorm'),
#'    var_type = c("lvl1", "lvl2"),
#'    opts = list(list(mean = 0, sd = 1.5),
#'    list(mean = 0, sd = 4)))
#' n <- 150
#' p <- 30
#' data_str <- "long"
#' temp_long <- sim_glm(fixed, random, random3 = NULL, fixed_param, 
#' random_param, random_param3 = NULL,
#'  cov_param, k = NULL, n, p, data_str = data_str)
#' 
#' 
#' # Three level example
#' fixed <- ~1 + time + diff + act + actClust + time:act
#' random <- ~1 + time + diff
#' random3 <- ~ 1 + time
#' fixed_param <- c(4, 2, 6, 2.3, 7, 0)
#' random_param <- list(random_var = c(7, 4, 2), rand_gen = 'rnorm')
#' random_param3 <- list(random_var = c(4, 2), rand_gen = 'rnorm')
#' cov_param <- list(dist_fun = c('rnorm', 'rnorm', 'rnorm'), 
#'    var_type = c("lvl1", "lvl2", "lvl3"),
#'    opts = list(list(mean = 0, sd = 1.5),
#'    list(mean = 0, sd = 4),
#'    list(mean = 0, sd = 2)))
#' k <- 10
#' n <- 15
#' p <- 10
#' data_str <- "long"
#' temp_three <- sim_glm(fixed, random, random3, fixed_param, random_param, 
#'   random_param3, cov_param, k,n, p, data_str = data_str)
#' 
#' 
sim_glm <- function(fixed, random, random3, fixed_param, random_param = list(), 
                    random_param3 = list(), cov_param, k, n, p, 
                    data_str, cor_vars = NULL, fact_vars = list(NULL), 
                    unbal = FALSE, unbal3 = FALSE, 
                    unbalCont = NULL, unbalCont3 = NULL) {
  
  if(data_str == "single"){
    sim_glm_single(fixed, fixed_param, cov_param, n, data_str, 
                   cor_vars, fact_vars)
  } else {
    if (is.null(k)){
      sim_glm_nested(fixed, random, fixed_param, random_param, cov_param, n, p, 
                     data_str, cor_vars, fact_vars, unbal, unbalCont)
    } else {
      sim_glm_nested3(fixed, random, random3, fixed_param, random_param, 
                      random_param3, cov_param, k, n, p, data_str, cor_vars, 
                      fact_vars, unbal, unbal3, unbalCont, unbalCont3)
    }
  }
}



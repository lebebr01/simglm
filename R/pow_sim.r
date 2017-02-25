#' Power simulation for nested designs
#' 
#' Takes simulation conditions as input, exports power.
#' 
#' Power function to compute power for a regression term for the linear mixed 
#' model. This function would need to be replicated to make any statement about 
#' power. Use \code{\link{sim_pow}} as a convenient wrapper for this.
#' 
#' @seealso \code{\link{sim_pow}} for a wrapper to replicate.
#' 
#' @param fixed One sided formula for fixed effects in the simulation. 
#'  To suppress intercept add -1 to formula.
#' @param random One sided formula for random effects in the simulation. 
#'  Must be a subset of fixed.
#' @param random3 One sided formula for random effects at third level in the 
#'  simulation. Must be a subset of fixed (and likely of random).
#' @param fixed_param Fixed effect parameter values (i.e. beta weights). 
#'  Must be same length as fixed.
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
#' @param k Number of third level clusters.
#' @param n Cluster sample size.
#' @param p Within cluster sample size.
#' @param error_var Scalar of error variance.
#' @param with_err_gen Simulated within cluster error distribution. 
#'  Must be a quoted 'r' distribution function.
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
#' @param unbal A vector of sample sizes for the number of observations for each
#'  level 2 cluster. Must have same length as level two sample size n. 
#'  Alternative specification can be TRUE, which uses additional argument, 
#'  unbalCont.
#' @param unbal3 A vector of sample sizes for the number of observations for 
#'  each level 3 cluster. Must have same length as level two sample size k. 
#'  Alternative specification can be TRUE, which uses additional argument, 
#'  unbalCont3.
#' @param unbalCont When unbal = TRUE, this specifies the minimum and maximum 
#'  level one size, will be drawn from a random uniform distribution with min 
#'  and max specified.
#' @param unbalCont3 When unbal3 = TRUE, this specifies the minimum and maximum 
#'  level two size, will be drawn from a random uniform distribution with min 
#'  and max specified.
#' @param lvl1_err_params Additional parameters passed as a list on to the 
#' level one error generating function
#' @param arima_mod A list indicating the ARIMA model to pass to arima.sim. 
#'             See \code{\link{arima.sim}} for examples.
#' @param missing TRUE/FALSE flag indicating whether missing data should be 
#'  simulated.
#' @param missing_args Additional missing arguments to pass to the missing_data 
#'  function. See \code{\link{missing_data}} for examples.
#' @param pow_param Name of variable to calculate power for, must be a name 
#'  from fixed.
#' @param alpha What should the per test alpha rate be used for the hypothesis 
#'  testing.
#' @param pow_dist Which distribution should be used when testing hypothesis 
#'  test, z or t?
#' @param pow_tail One-tailed or two-tailed test?
#' @param ... Not currently used.
#' @export 
sim_pow_nested3 <- function(fixed, random, random3, fixed_param, 
                            random_param = list(), random_param3 = list(), 
                            cov_param, k, n, p, error_var, with_err_gen, 
                            arima = FALSE, data_str, cor_vars = NULL, 
                            fact_vars = list(NULL), unbal = FALSE, 
                            unbal3 = FALSE, unbalCont = NULL, unbalCont3 = NULL,
                            lvl1_err_params = NULL, arima_mod = list(NULL), 
                            missing = FALSE, missing_args = list(NULL),
                           pow_param = NULL, alpha, pow_dist = c("z", "t"), 
                           pow_tail = c(1, 2), ...) {
  
  fixed_vars <- attr(terms(fixed),"term.labels")   
  rand_vars <- attr(terms(random),"term.labels")
  rand_vars3 <- attr(terms(random3), 'term.labels')
  if(length(rand_vars) == 0) {
    rand_vars <- 1
  }
  
  if(any(pow_param %ni% c(fixed_vars, '(Intercept)'))) { 
    stop('pow_param must be a subset of fixed')
  }
  
  temp_nest <- sim_reg_nested3(fixed, random, random3, fixed_param, random_param, 
                               random_param3, cov_param, k, n, p, error_var, 
                               with_err_gen, arima, data_str, cor_vars, 
                               fact_vars, unbal, unbal3, unbalCont, unbalCont3, 
                               lvl1_err_params, arima_mod, ...)
  if(missing) {
    temp_nest <- do.call(missing_data, c(list(sim_data = temp_nest), 
                                         missing_args))
  }
  
  if(arima) {
    # fix1 <- paste("sim_data ~", paste(fixed_vars, collapse = "+"))
    # ran1 <- paste("~", paste(rand_vars, collapse = "+"), "|clustID", sep = "")
    # 
    # temp_mod <- nlme::lme(fixed = as.formula(fix1), data = temp_nest, 
    # random = as.formula(ran1))
    # test_stat <- data.frame(abs(summary(temp_mod)$coefficients$fixed))
  } else {
    fix1 <- paste("sim_data ~", paste(fixed_vars, collapse = "+"))
    if(missing) {
      fix1 <- gsub('sim_data', 'sim_data2', fix1)
    }
    ran1 <- paste("(", paste(rand_vars, collapse = "+"), "| clustID)", sep = "")
    if(length(rand_vars3) == 0) {
      ran2 <- '(1 | clust3ID)'
    } else {
      ran2 <- paste('(', paste(rand_vars3, collapse = "+"), "| clust3ID)", 
                    sep = "")
    }
    fm1 <- as.formula(paste(fix1, ran1, ran2, sep = "+ "))
    
    temp_mod <- lme4::lmer(fm1, data = temp_nest)
    test_stat <- data.frame(abs(summary(temp_mod)$coefficients[, 3]))
  }
  
  crit <- qnorm(alpha/pow_tail, lower.tail = FALSE)
  
  if(is.null(pow_param)) {
    pow_param <- rownames(test_stat)
  } else {
    test_stat <- test_stat[pow_param, ]
  }
  
  reject <- data.frame(var = pow_param,
                       test_stat = test_stat)
  reject$reject <- ifelse(test_stat >= crit, 1, 0)
  
  reject
}

#' Power simulation for nested designs
#' 
#' Takes simulation conditions as input, exports power.
#' 
#' Power function to compute power for a regression term for the linear 
#' mixed model. This function would need to be replicated to make any statement 
#' about power.  Use \code{\link{sim_pow}} as a convenient wrapper for this.
#' 
#' @seealso \code{\link{sim_pow}} for a wrapper to replicate.
#' 
#' @param fixed One sided formula for fixed effects in the simulation. 
#'  To suppress intercept add -1 to formula.
#' @param random One sided formula for random effects in the simulation. 
#'  Must be a subset of fixed.
#' @param fixed_param Fixed effect parameter values (i.e. beta weights). 
#'  Must be same length as fixed.
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
#' @param n Cluster sample size.
#' @param p Within cluster sample size.
#' @param error_var Scalar of error variance.
#' @param with_err_gen Simulated within cluster error distribution. 
#'  Must be a quoted 'r' distribution function.
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
#' @param unbalCont When unbal = TRUE, this specifies the minimum and maximum 
#'  level one size, will be drawn from a random uniform distribution with min 
#'  and max specified.
#' @param lvl1_err_params Additional parameters passed as a list on to the level
#'  one error generating function
#' @param arima_mod A list indicating the ARIMA model to pass to arima.sim. 
#'             See \code{\link{arima.sim}} for examples.
#' @param missing TRUE/FALSE flag indicating whether missing data should be 
#'  simulated.
#' @param missing_args Additional missing arguments to pass to the missing_data 
#'  function. See \code{\link{missing_data}} for examples.
#' @param pow_param Name of variable to calculate power for, must be a name 
#'  from fixed.
#' @param alpha What should the per test alpha rate be used for the hypothesis 
#'  testing.
#' @param pow_dist Which distribution should be used when testing hypothesis 
#'  test, z or t?
#' @param pow_tail One-tailed or two-tailed test?
#' @param ... Not currently used.
#' @export 
sim_pow_nested <- function(fixed, random, fixed_param, random_param = list(), 
                        cov_param, n, p, error_var, with_err_gen, arima = FALSE, 
                        data_str, cor_vars = NULL, fact_vars = list(NULL),
                        unbal = FALSE, unbalCont = NULL, lvl1_err_params = NULL,
                        arima_mod = list(NULL), missing = FALSE, 
                        missing_args = list(NULL), pow_param = NULL, alpha, 
                        pow_dist = c("z", "t"), pow_tail = c(1, 2), ...) {
  
  fixed_vars <- attr(terms(fixed),"term.labels")    
  rand_vars <- attr(terms(random),"term.labels")
  if(length(rand_vars) == 0) {
    rand_vars <- 1
  }
  
  if(any(pow_param %ni% c(fixed_vars, '(Intercept)'))) { 
    stop('pow_param must be a subset of fixed')
  }

  temp_nest <- sim_reg_nested(fixed, random, fixed_param, random_param, 
                              cov_param, n, p, error_var, with_err_gen, arima,
                              data_str, cor_vars, fact_vars, unbal, unbalCont,
                              lvl1_err_params, arima_mod, ...)
  if(missing) {
    temp_nest <- do.call(missing_data, c(list(sim_data = temp_nest), 
                                         missing_args))
  }
  
  if(arima) {
    # fix1 <- paste("sim_data ~", paste(fixed_vars, collapse = "+"))
    # ran1 <- paste("~", paste(rand_vars, collapse = "+"), "|clustID", sep = "")
    # 
    # temp_mod <- nlme::lme(fixed = as.formula(fix1), data = temp_nest, 
    # random = as.formula(ran1))
    # test_stat <- data.frame(abs(summary(temp_mod)$coefficients$fixed))
  } else {
    fix1 <- paste("sim_data ~", paste(fixed_vars, collapse = "+"))
    if(missing) {
      fix1 <- gsub('sim_data', 'sim_data2', fix1)
    }
    ran1 <- paste("(", paste(rand_vars, collapse = "+"), "|clustID)", sep = "")
    fm1 <- as.formula(paste(fix1, ran1, sep = "+ "))
    
    temp_mod <- lme4::lmer(fm1, data = temp_nest)
    test_stat <- data.frame(abs(summary(temp_mod)$coefficients[, 3]))
  }
  
  crit <- qnorm(alpha/pow_tail, lower.tail = FALSE)
  
  if(is.null(pow_param)) {
    pow_param <- rownames(test_stat)
  } else {
    test_stat <- test_stat[pow_param, ]
  }
  
  reject <- data.frame(var = pow_param,
                       test_stat = test_stat)
  reject$reject <- ifelse(test_stat >= crit, 1, 0)
  
  reject
}


#' Function to simulate power.
#' 
#' Input simulation conditions and which term to compute power for, export 
#' reported power.
#' 
#' Power function to compute power for a regression term for simple regression 
#' models. This function would need to be replicated to make any statement about
#' power. Use \code{\link{sim_pow}} as a convenient wrapper for this.
#' 
#' @seealso \code{\link{sim_pow}} for a wrapper to replicate.
#' 
#' @param fixed One sided formula for fixed effects in the simulation. 
#'  To suppress intercept add -1 to formula.
#' @param fixed_param Fixed effect parameter values (i.e. beta weights). 
#'  Must be same length as fixed.
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
#' @param n Cluster sample size.
#' @param error_var Scalar of error variance.
#' @param with_err_gen Simulated within cluster error distribution. Must be a 
#'  quoted 'r' distribution function.
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
#' @param lvl1_err_params Additional parameters passed as a list on to the 
#'   level one error generating function
#' @param arima_mod A list indicating the ARIMA model to pass to arima.sim. 
#'             See \code{\link{arima.sim}} for examples.
#' @param missing TRUE/FALSE flag indicating whether missing data should be 
#'   simulated.
#' @param missing_args Additional missing arguments to pass to the missing_data 
#'   function. See \code{\link{missing_data}} for examples.
#' @param pow_param Name of variable to calculate power for, must be a name from
#'  fixed.
#' @param alpha What should the per test alpha rate be used for the hypothesis 
#'  testing.
#' @param pow_dist Which distribution should be used when testing hypothesis 
#'  test, z or t?
#' @param pow_tail One-tailed or two-tailed test?
#' @param ... Additional specification needed to pass to the random generating 
#'             function defined by with_err_gen.
#' @export 
sim_pow_single <- function(fixed, fixed_param, cov_param, n, error_var, 
                      with_err_gen, arima = FALSE, data_str, cor_vars = NULL, 
                      fact_vars = list(NULL), lvl1_err_params = NULL, 
                      arima_mod = list(NULL), missing = FALSE, 
                      missing_args = list(NULL), pow_param = NULL, alpha, 
                      pow_dist = c("z", "t"), pow_tail = c(1, 2), ...) {
  
  fixed_vars <- attr(terms(fixed),"term.labels")
  
  if(any(pow_param %ni% c(fixed_vars, '(Intercept)', 'Intercept'))) { 
    stop('pow_param must be a subset of fixed')
  }
  
  temp_single <- sim_reg_single(fixed, fixed_param, cov_param, n, error_var, 
                                with_err_gen, arima, data_str, 
                                cor_vars, fact_vars, lvl1_err_params, arima_mod,
                                ...)

  fm1 <- as.formula(paste("sim_data ~", paste(fixed_vars, collapse = "+")))
  if(missing) {
    temp_single <- do.call(missing_data, c(list(sim_data = temp_single), 
                                           missing_args))
    fm1 <- as.formula(paste("sim_data2 ~", paste(fixed_vars, collapse = "+")))
  }
  
  temp_lm <- lm(fm1, data = temp_single)
  
  crit <- ifelse(pow_dist == "z", qnorm(alpha/pow_tail, lower.tail = FALSE), 
                qt(alpha/pow_tail, df = nrow(temp_single) - length(fixed_param),
                   lower.tail = FALSE))
  test_stat <- data.frame(abs(coefficients(summary(temp_lm))[, 3]))

  if(is.null(pow_param)) {
    pow_param <- rownames(test_stat)
  } else {
    test_stat <- test_stat[pow_param, ]
  }
  
  reject <- data.frame(var = pow_param,
                       test_stat = test_stat)
  reject$reject <- ifelse(test_stat >= crit, 1, 0)
  
  reject
}

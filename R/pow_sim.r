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
#'      \item  random_var: variance of random parameters,
#'      \item  rand_gen: Name of simulation function for random effects.
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
#'        \item random_var: variance of random parameters,
#'        \item rand_gen: Name of simulation function for random effects.
#'    }
#'          Optional elements are:
#'    \itemize{
#'        \item ther: Theorectial mean and variance from rand_gen,
#'        \item ther_sim: Simulate mean/variance for standardization purposes,
#'        \item cor_vars: Correlation between random effects,
#'        \item ...: Additional parameters needed for rand_gen function.
#'    }
#' @param cov_param List of arguments to pass to the continuous generating 
#'   function, must be the same order as the variables specified in fixed. 
#'   This list does not include intercept, time, factors, or 
#'   interactions. Required arguments include:
#'   \itemize{
#'     \item dist_fun: This is a quoted R distribution function.
#'     \item var_type: This is the level of variable to generate. Must be 
#'       'level1', 'level2', or 'level3'. 
#'       Must be same order as fixed formula above.
#'   }
#'   Optional arguments to the distribution functions are in a nested list,
#'    see the examples or vignettes for example code.
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
#'        \item numlevels: Number of levels for ordinal or factor variables.
#'        \item var_type: Must be 'level1', 'level2', or 'level3'.
#'    }
#'    Optional arguments include:
#'    \itemize{
#'        \item replace
#'        \item prob
#'        \item value.labels
#'    }
#'     See also \code{\link{sample}} for use of these optional arguments.
#' @param unbal A named TRUE/FALSE list specifying whether unbalanced simulation 
#'  design is desired. The named elements must be: "level2" or "level3" representing
#'  unbalanced simulation for level two and three respectively. Default is FALSE,
#'  indicating balanced sample sizes at both levels.
#' @param unbal_design When unbal = TRUE, this specifies the design for unbalanced
#'  simulation in one of two ways. It can represent the minimum and maximum 
#'  sample size within a cluster via a named list. This will be drawn from a 
#'  random uniform distribution with min and max specified. 
#'  Secondly, the actual sample sizes within each cluster
#'  can be specified. This takes the form of a vector that must have the same length 
#'  as the level two or three sample size. These are specified as a named list in which
#'  level two sample size is controlled via "level2" and level three sample size is 
#'  controlled via "level3".
#' @param lvl1_err_params Additional parameters passed as a list on to the 
#' level one error generating function
#' @param arima_mod A list indicating the ARIMA model to pass to arima.sim. 
#'             See \code{\link{arima.sim}} for examples.
#' @param contrasts An optional list that specifies the contrasts to be used 
#'  for factor variables (i.e. those variables with .f or .c). 
#'  See \code{\link{contrasts}} for more detail.
#' @param homogeneity Either TRUE (default) indicating homogeneity of variance
#'  assumption is assumed or FALSE to indicate desire to generate heterogeneity 
#'  of variance.
#' @param heterogeneity_var Variable name as a character string to use for 
#'  heterogeneity of variance simulation.
#' @param cross_class_params A list of named parameters when cross classified 
#'  data structures are desired. Must include the following arguments:
#'   \itemize{
#'    \item num_ids: The number of cross classified clusters. These are in 
#'         addition to the typical cluster ids
#'    \item random_param: This argument is a list of arguments passed to 
#'       \code{\link{sim_rand_eff}}. These must include:
#'      \itemize{
#'       \item random_var: The variance of the cross classified random effect
#'       \item rand_gen: The random generating function used to generate the 
#'          cross classified random effect.
#'      }
#'      Optional elements are:
#'    \itemize{
#'        \item ther: Theorectial mean and variance from rand_gen,
#'        \item ther_sim: Simulate mean/variance for standardization purposes,
#'        \item cor_vars: Correlation between random effects,
#'        \item ...: Additional parameters needed for rand_gen function.
#'    } 
#'   }
#' @param knot_args A nested list of named knot arguments. See \code{\link{sim_knot}} 
#'  for more details. Arguments must include:
#'    \itemize{
#'      \item var
#'      \item knot_locations
#'    }
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
#' @param lme4_fit_mod Valid lme4 syntax to be used for model fitting.
#' @param nlme_fit_mod Valid nlme syntax to be used for model fitting. 
#'   This should be specified as a named list with fixed and random components.
#' @param arima_fit_mod Valid nlme syntax for fitting serial correlation structures.
#'   See \code{\link{corStruct}} for help. This must be specified to 
#'   include serial correlation.
#' @param general_mod Valid model syntax. This syntax can be from any R package. 
#'   By default, broom is used to extract model result information. Note, 
#'   package must be defined or loaded prior to running the sim_pow function.
#' @param general_extract A valid function to extract model results if 
#'   general_mod argument is used. This argument is primarily used if extracting model
#'   results is not possibly using the broom package. If this is left NULL (default), 
#'   broom is used to collect model results.
#' @param ... Not currently used.
#' @importFrom purrr is_formula
#' @export 
sim_pow_nested3 <- function(fixed, random, random3, fixed_param, 
                            random_param = list(), random_param3 = list(), 
                            cov_param, k, n, p, error_var, with_err_gen, 
                            arima = FALSE, data_str, cor_vars = NULL, 
                            fact_vars = list(NULL), 
                            unbal = list("level2" = FALSE, "level3" = FALSE), 
                            unbal_design = list("level2" = NULL, "level3" = NULL),
                            lvl1_err_params = NULL, arima_mod = list(NULL), 
                            contrasts = NULL, homogeneity = TRUE,
                            heterogeneity_var = NULL, cross_class_params = NULL,
                            knot_args = list(NULL),
                            missing = FALSE, missing_args = list(NULL),
                            pow_param = NULL, alpha, pow_dist = c("z", "t"), 
                            pow_tail = c(1, 2), lme4_fit_mod = NULL, 
                            nlme_fit_mod = NULL, arima_fit_mod = NULL, 
                            general_mod = NULL, general_extract = NULL, ...) {
  
  fixed_vars <- attr(terms(fixed),"term.labels")   
  rand_vars <- attr(terms(random),"term.labels")
  rand_vars3 <- attr(terms(random3), 'term.labels')
  if(length(rand_vars) == 0) {
    rand_vars <- 1
  }
  
  if(any(pow_param %ni% c(fixed_vars, '(Intercept)'))) { 
    stop('pow_param must be a subset of fixed')
  }
  
  data <- sim_reg_nested3(fixed, random, random3, fixed_param, random_param, 
                               random_param3, cov_param, k, n, p, error_var, 
                               with_err_gen, arima, data_str, cor_vars, 
                               fact_vars, unbal, unbal_design, 
                               lvl1_err_params, arima_mod, contrasts, 
                               homogeneity, heterogeneity_var, 
                               cross_class_params, knot_args, ...)
  if(missing) {
    data <- do.call(missing_data, c(list(sim_data = data), 
                                         missing_args))
  }
  
  if(!is.null(lme4_fit_mod)) {
    if(!purrr::is_formula(lme4_fit_mod)) {
      stop('lme4_fit_mod must be a formula to pass to lmer')
    }
    temp_mod <- lme4::lmer(lme4_fit_mod, data = data)
  } else {
    if(!is.null(nlme_fit_mod)) {
      if(all(unlist(lapply(nlme_fit_mod, purrr::is_formula)))) {
        temp_mod <- nlme::lme(fixed = nlme_fit_mod$fixed, data = data,
                              random = nlme_fit_mod$random)
        test_stat <- data.frame(abs(summary(temp_mod)$coefficients$fixed))
      } else {
        if(!purrr::is_formula(nlme_fit_mod$fixed)) {
          stop('nlme_fit_mod$fixed must be a formula to pass to lme')
        } 
        temp_mod <- nlme::lme(fixed = nlme_fit_mod$fixed, data = data,
                              random = eval(parse(text = nlme_fit_mod$random)), 
                              correlation = arima_fit_mod)
      }
    } else {
      if(!is.null(general_mod)) {
        temp_mod <- eval(parse(text = general_mod))
      } else {
        if(arima) {
          fix1 <- paste("sim_data ~", paste(fixed_vars, collapse = "+"))
          if(missing) {
            fix1 <- gsub('sim_data', 'sim_data2', fix1)
          }
          ran1 <- paste("list(clustID =~", paste(rand_vars, collapse = "+"), sep = "")
          if(length(rand_vars3) == 0) {
            ran2 <- 'clust3ID = ~ 1)'
          } else {
            ran2 <- paste('clust3ID = ~', paste(rand_vars3, collapse = "+"), ')',  
                          sep = "")
          }
          ran <- paste(ran1, ran2, collapse = ', ')
          
          temp_mod <- nlme::lme(fixed = as.formula(fix1), data = data,
                                random = eval(parse(text = ran)),
                                correlation = arima_fit_mod)
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
          
          temp_mod <- lme4::lmer(fm1, data = data)
        }
      }
    }
  }
    
  if(!is.null(general_extract)) {
    test_stat <- do.call(general_extract, temp_mod)
  } else{
    test_stat <- broom::tidy(temp_mod, effects = 'fixed')
  }
  
  crit <- qnorm(alpha/pow_tail, lower.tail = FALSE)
  
  if(!is.null(pow_param)) {
    test_stat <- dplyr::filter(test_stat, term %in% pow_param)
  }
  
  test_stat['reject'] <- c(ifelse(test_stat['estimate'] >= crit, 1, 0))
  
  test_stat
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
#'      \item  random_var: variance of random parameters,
#'      \item  rand_gen: Name of simulation function for random effects.
#'   }
#'          Optional elements are:
#'   \itemize{
#'      \item ther: Theorectial mean and variance from rand_gen,
#'      \item ther_sim: Simulate mean/variance for standardization purposes,
#'      \item cor_vars: Correlation between random effects,
#'      \item ...: Additional parameters needed for rand_gen function.
#'    }
#' @param cov_param List of arguments to pass to the continuous generating 
#'   function, must be the same order as the variables specified in fixed. 
#'   This list does not include intercept, time, factors, or 
#'   interactions. Required arguments include:
#'   \itemize{
#'     \item dist_fun: This is a quoted R distribution function.
#'     \item var_type: This is the level of variable to generate. Must be 
#'       'level1' or 'level2'. 
#'       Must be same order as fixed formula above.
#'   }
#'   Optional arguments to the distribution functions are in a nested list,
#'    see the examples or vignettes for example code.
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
#'        \item numlevels: Number of levels for ordinal or factor variables.
#'        \item var_type: Must be 'level1' or 'level2'.
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
#'  unbal_design.
#' @param unbal_design When unbal = TRUE, this specifies the design for unbalanced
#'  simulation in one of two ways. It can represent the minimum and maximum 
#'  sample size within a cluster via a named list. This will be drawn from a 
#'  random uniform distribution with min and max specified. 
#'  Secondly, the sample sizes within each cluster can be specified. 
#'  This takes the form of a vector that must have the same length 
#'  as the level two sample size.
#' @param lvl1_err_params Additional parameters passed as a list on to the level
#'  one error generating function
#' @param arima_mod A list indicating the ARIMA model to pass to arima.sim. 
#'             See \code{\link{arima.sim}} for examples.
#' @param contrasts An optional list that specifies the contrasts to be used 
#'  for factor variables (i.e. those variables with .f or .c). 
#'  See \code{\link{contrasts}} for more detail.
#' @param homogeneity Either TRUE (default) indicating homogeneity of variance
#'  assumption is assumed or FALSE to indicate desire to generate heterogeneity 
#'  of variance.
#' @param heterogeneity_var Variable name as a character string to use for 
#'  heterogeneity of variance simulation.
#' @param cross_class_params A list of named parameters when cross classified 
#'  data structures are desired. Must include the following arguments:
#'   \itemize{
#'    \item num_ids: The number of cross classified clusters. These are in 
#'         addition to the typical cluster ids
#'    \item random_param: This argument is a list of arguments passed to 
#'       \code{\link{sim_rand_eff}}. These must include:
#'      \itemize{
#'       \item random_var: The variance of the cross classified random effect
#'       \item rand_gen: The random generating function used to generate the 
#'          cross classified random effect.
#'      }
#'      Optional elements are:
#'    \itemize{
#'        \item ther: Theorectial mean and variance from rand_gen,
#'        \item ther_sim: Simulate mean/variance for standardization purposes,
#'        \item cor_vars: Correlation between random effects,
#'        \item ...: Additional parameters needed for rand_gen function.
#'    } 
#'   }
#' @param knot_args A nested list of named knot arguments. See \code{\link{sim_knot}} 
#'  for more details. Arguments must include:
#'    \itemize{
#'      \item var
#'      \item knot_locations
#'    }
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
#' @param lme4_fit_mod Valid lme4 syntax to be used for model fitting.
#' @param nlme_fit_mod Valid nlme syntax to be used for model fitting. 
#'   This should be specified as a named list with fixed and random components.
#' @param arima_fit_mod Valid nlme syntax for fitting serial correlation structures.
#'   See \code{\link{corStruct}} for help. This must be specified to 
#'   include serial correlation.
#' @param general_mod Valid model syntax. This syntax can be from any R package. 
#'   By default, broom is used to extract model result information. Note, 
#'   package must be defined or loaded prior to running the sim_pow function.
#' @param general_extract A valid function to extract model results if 
#'   general_mod argument is used. This argument is primarily used if extracting model
#'   results is not possibly using the broom package. If this is left NULL (default), 
#'   broom is used to collect model results.
#' @param ... Not currently used.
#' @export 
sim_pow_nested <- function(fixed, random, fixed_param, random_param = list(), 
                        cov_param, n, p, error_var, with_err_gen, arima = FALSE, 
                        data_str, cor_vars = NULL, fact_vars = list(NULL),
                        unbal = FALSE, unbal_design = NULL, lvl1_err_params = NULL,
                        arima_mod = list(NULL), contrasts = NULL, 
                        homogeneity = TRUE, heterogeneity_var = NULL, 
                        cross_class_params = NULL, knot_args = list(NULL),
                        missing = FALSE, 
                        missing_args = list(NULL), pow_param = NULL, alpha, 
                        pow_dist = c("z", "t"), pow_tail = c(1, 2), 
                        lme4_fit_mod = NULL, 
                        nlme_fit_mod = NULL, arima_fit_mod = NULL, 
                        general_mod = NULL, general_extract = NULL, ...) {
  
  fixed_vars <- attr(terms(fixed),"term.labels")    
  rand_vars <- attr(terms(random),"term.labels")
  if(length(rand_vars) == 0) {
    rand_vars <- 1
  }
  
  if(any(pow_param %ni% c(fixed_vars, '(Intercept)'))) { 
    stop('pow_param must be a subset of fixed')
  }
  
  data <- sim_reg_nested(fixed, random, fixed_param, random_param, 
                         cov_param, n, p, error_var, with_err_gen, arima,
                         data_str, cor_vars, fact_vars, unbal, unbal_design,
                         lvl1_err_params, arima_mod, contrasts, 
                         homogeneity, heterogeneity_var, 
                         cross_class_params, knot_args, ...)
  if(missing) {
    data <- do.call(missing_data, c(list(sim_data = data), 
                                         missing_args))
  }
  
  if(!is.null(lme4_fit_mod)) {
    if(!purrr::is_formula(lme4_fit_mod)) {
      stop('lme4_fit_mod must be a formula to pass to lmer')
    }
    temp_mod <- lme4::lmer(lme4_fit_mod, data = data)
  } else {
    if(!is.null(nlme_fit_mod)) {
      if(all(unlist(lapply(nlme_fit_mod, purrr::is_formula)))) {
        temp_mod <- nlme::lme(fixed = nlme_fit_mod$fixed, data = data,
                              random = nlme_fit_mod$random, 
                              correlation = arima_fit_mod)
      } else {
        if(!purrr::is_formula(nlme_fit_mod$fixed)) {
          stop('nlme_fit_mod$fixed must be a formula to pass to lme')
        } 
        temp_mod <- nlme::lme(fixed = nlme_fit_mod$fixed, data = data,
                              random = eval(parse(text = nlme_fit_mod$random)), 
                              correlation = arima_fit_mod)
      }
    } else {
      if(!is.null(general_mod)) {
        temp_mod <- eval(parse(text = general_mod))
      } else {
        if(arima) {
          fix1 <- paste("sim_data ~", paste(fixed_vars, collapse = "+"))
          if(missing) {
            fix1 <- gsub('sim_data', 'sim_data2', fix1)
          }
          ran1 <- paste("~", paste(rand_vars, collapse = "+"), "|clustID", sep = "")
          
          temp_mod <- nlme::lme(fixed = as.formula(fix1), data = data,
                                random = as.formula(ran1), 
                                correlation = arima_fit_mod)
        } else {
          fix1 <- paste("sim_data ~", paste(fixed_vars, collapse = "+"))
          if(missing) {
            fix1 <- gsub('sim_data', 'sim_data2', fix1)
          }
          ran1 <- paste("(", paste(rand_vars, collapse = "+"), "|clustID)", sep = "")
          fm1 <- as.formula(paste(fix1, ran1, sep = "+ "))
          
          temp_mod <- lme4::lmer(fm1, data = data)
        }
      }
    }
  }
  if(!is.null(general_extract)) {
    test_stat <- do.call(general_extract, temp_mod)
  } else{
      test_stat <- broom::tidy(temp_mod, effects = 'fixed')
  }
  
  crit <- qnorm(alpha/pow_tail, lower.tail = FALSE)
  
  if(!is.null(pow_param)) {
    test_stat <- dplyr::filter(test_stat, term %in% pow_param)
  }
  
  test_stat['reject'] <- c(ifelse(test_stat['estimate'] >= crit, 1, 0))
  
  test_stat
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
#'   function, must be the same order as the variables specified in fixed. 
#'   This list does not include intercept, time, factors, or 
#'   interactions. Required arguments include:
#'   \itemize{
#'     \item dist_fun: This is a quoted R distribution function.
#'     \item var_type: This is the level of variable to generate. Must be 
#'       'single'. 
#'       Must be same order as fixed formula above.
#'   }
#'   Optional arguments to the distribution functions are in a nested list,
#'    see the examples or vignettes for example code.
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
#'        \item numlevels: Number of levels for ordinal or factor variables.
#'        \item var_type: Must be 'single'.
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
#' @param contrasts An optional list that specifies the contrasts to be used 
#'  for factor variables (i.e. those variables with .f or .c). 
#'  See \code{\link{contrasts}} for more detail.
#' @param homogeneity Either TRUE (default) indicating homogeneity of variance
#'  assumption is assumed or FALSE to indicate desire to generate heterogeneity 
#'  of variance.
#' @param heterogeneity_var Variable name as a character string to use for 
#'  heterogeneity of variance simulation.
#' @param knot_args A nested list of named knot arguments. See \code{\link{sim_knot}} 
#'  for more details. Arguments must include:
#'    \itemize{
#'      \item var
#'      \item knot_locations
#'    }
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
#' @param lm_fit_mod Valid lm syntax to be used for model fitting.
#' @param general_mod Valid model syntax. This syntax can be from any R package. 
#'   By default, broom is used to extract model result information. Note, 
#'   package must be defined or loaded prior to running the sim_pow function.
#' @param general_extract A valid function to extract model results if 
#'   general_mod argument is used. This argument is primarily used if extracting model
#'   results is not possibly using the broom package. If this is left NULL (default), 
#'   broom is used to collect model results.
#' @param ... Additional specification needed to pass to the random generating 
#'             function defined by with_err_gen.
#' @export 
#' @importFrom broom tidy
sim_pow_single <- function(fixed, fixed_param, cov_param, n, error_var, 
                      with_err_gen, arima = FALSE, data_str, cor_vars = NULL, 
                      fact_vars = list(NULL), lvl1_err_params = NULL, 
                      arima_mod = list(NULL), contrasts = NULL, 
                      homogeneity = TRUE, heterogeneity_var = NULL, 
                      knot_args = list(NULL), missing = FALSE, 
                      missing_args = list(NULL), pow_param = NULL, alpha, 
                      pow_dist = c("z", "t"), pow_tail = c(1, 2), 
                      lm_fit_mod = NULL, general_mod = NULL, 
                      general_extract = NULL, ...) {
  
  fixed_vars <- attr(terms(fixed),"term.labels")
  
  data <- sim_reg_single(fixed, fixed_param, cov_param, n, error_var, 
                         with_err_gen, arima, data_str, 
                         cor_vars, fact_vars, lvl1_err_params, arima_mod,
                         contrasts, homogeneity, heterogeneity_var,
                         knot_args, ...)
  
  if(missing) {
    data <- do.call(missing_data, c(list(sim_data = data), 
                                    missing_args))
  }

  if(!is.null(lm_fit_mod)) {
    if(!purrr::is_formula(lm_fit_mod)) {
      stop('lm_fit_mod must be a formula to pass to lm')
    }
    temp_lm <- lm(lm_fit_mod, data = data)
  } else {
    if(!is.null(general_mod)) {
      temp_lm <- eval(parse(text = general_mod))
    } else {
      fm1 <- as.formula(paste("sim_data ~", paste(fixed_vars, collapse = "+")))
      if(missing) {
        fm1 <- as.formula(paste("sim_data2 ~", paste(fixed_vars, collapse = "+")))
      }
      
      temp_lm <- lm(fm1, data = data)
    }
  }
  if(!is.null(general_extract)) {
    test_stat <- do.call(general_extract, temp_lm)
  } else{
    test_stat <- broom::tidy(temp_lm)
  }
  
  crit <- ifelse(pow_dist == "z", qnorm(alpha/pow_tail, lower.tail = FALSE), 
                qt(alpha/pow_tail, df = nrow(data) - length(fixed_param),
                   lower.tail = FALSE))

  if(!is.null(pow_param)) {
    test_stat <- dplyr::filter(test_stat, term %in% pow_param)
  }
  
  test_stat['reject'] <- c(ifelse(test_stat['estimate'] >= crit, 1, 0))
  
  test_stat
}

#' Tidy Model Fitting Function
#' 
#' @param data A data object, most likely generated from within simglm
#' @param sim_args A named list with special model formula syntax. See details and examples
#'   for more information. The named list may contain the following:
#'   \itemize{
#'     \item fixed: This is the fixed portion of the model (i.e. covariates)
#'     \item random: This is the random portion of the model (i.e. random effects)
#'     \item error: This is the error (i.e. residual term).
#'     \item model_fit: These are arguments passed to the \code{\link{model_fit}}
#'       function. 
#'   }
#' @param ... Additional arguments needed to pass on to model fitting 
#'   functions. See specific model fitting functions for specific details.
#' @examples 
#' 
#' @export 
model_fit <- function(data, sim_args, ...) {
  
  if(is.null(sim_args$model_fit$model_function)) {
    if(length(parse_formula(sim_args)$randomeffect) == 0) {
      model_function <- 'lm'
    } else {
      model_function <- 'lmer'
    }
  } else {
    model_function <- sim_args$model_fit$model_function
  }
  if(is.null(sim_args$model_fit$formula)) {
    formula <- sim_args$formula
  } else {
    formula <- sim_args$model_fit$formula
  }
  
  purrr::invoke(model_function, 
                list(formula = formula, data = data, ...))
}

#' Extract Coefficients
#' 
#' @param model A returned model object from a fitted model.
#' @param extract_function A function that extracts model results. The 
#'   function must take the model object as the only argument.
#' @export 
extract_coefficients <- function(model, extract_function = NULL) {
  
  if(is.null(extract_function)) {
    broom::tidy(model)
  } else {
    purrr::invoke(extract_function, model)
  }
}



#' Replicate Simulation
#' 
#' @param sim_args A named list with special model formula syntax. See details and examples
#'   for more information. The named list may contain the following:
#'   \itemize{
#'     \item fixed: This is the fixed portion of the model (i.e. covariates)
#'     \item random: This is the random portion of the model (i.e. random effects)
#'     \item error: This is the error (i.e. residual term).
#'   }
#' @param expression Simulation, model fitting, and coefficient extraction
#'   expressions for a single replication.
#' @param ... Currently not used.
#' @importFrom purrr rerun
#' @importFrom dplyr enquo
#' @importFrom dplyr quo
#' @examples 
#' 
#' @export 
replicate_simulation <- function(sim_args, expression, ...) {
  
  expression_quo <- dplyr::enquo(expression)
  purrr::rerun(sim_args[['replications']], !!expression_quo)
  
}

#' Function to replication simulation with varying arguments
#' 
#' This is a wrapper around \code{\link{simglm}} master function.
#' 
#' @param sim_args A nested named list with special model formula syntax. See details and examples
#'   for more information. The named list may contain the following:
#'   \itemize{
#'     \item fixed: This is the fixed portion of the model (i.e. covariates)
#'     \item random: This is the random portion of the model (i.e. random effects)
#'     \item error: This is the error (i.e. residual term).
#'   }
#'   
#' @export
replicate_simulation_vary <- function(sim_args) {
  
  conditions <- data.frame(sapply(expand.grid(sim_args[['vary_arguments']], KEEP.OUT.ATTRS = FALSE),
                                  as.character))
  
  power_out <- lapply(seq_along(sim_args), function(xx) 
      data.frame(conditions[xx, , drop = FALSE], 
            dplyr::bind_rows(
              purrr::rerun(sim_args[[xx]][['replications']], simglm(sim_args[[xx]]))
            ),
            row.names = NULL
      )
    )
  
  power_out
  
}

#' Compute Power, Type I Error, or Precision Statistics
#' 
#' @param data A list of model results generated by \code{\link{replicate_simulation}}
#'  function.
#' @param sim_args A named list with special model formula syntax. See details and examples
#'   for more information. The named list may contain the following:
#'   \itemize{
#'     \item fixed: This is the fixed portion of the model (i.e. covariates)
#'     \item random: This is the random portion of the model (i.e. random effects)
#'     \item error: This is the error (i.e. residual term).
#'   }
#' @param power TRUE/FALSE flag indicating whether power should be computed. 
#'  Defaults to TRUE.
#' @param type_1_error TRUE/FALSE flag indicating whether type I error rate
#'  should be computed. Defaults to TRUE.
#' @param precision TRUE/FALSE flag indicating whether precision should be 
#'  computed. Defaults to TRUE.
#' @importFrom dplyr mutate
#' @import rlang
#' @export
compute_statistics <- function(data,  sim_args, power = TRUE, 
                               type_1_error = TRUE, precision = TRUE) {
  
  data_df <- data %>%
    purrr::map(compute_power, sim_args) %>% 
    purrr::map(compute_t1e, sim_args) %>%
    dplyr::bind_rows()
  
  if(is.null(sim_args['vary_arguments'])) {
    group_vars <- c('term')
  } else {
    group_vars <- c(names(expand.grid(sim_args[['vary_arguments']], KEEP.OUT.ATTRS = FALSE)),
                       'term')
  }
  
  if(power) {
    power_computation <- aggregate_power(data_df, 
                                         rlang::syms(group_vars))
  } else {
    power_computation <- NULL
  }
  
  if(type_1_error) {
    type_1_error_computation <- aggregate_t1e(data_df, 
                                              rlang::syms(group_vars))
  } else {
    type_1_error_computation <- NULL
  }
  
  if(precision) {
    precision_computation <- aggregate_precision(data_df, 
                                                 rlang::syms(group_vars))
  } else {
    precision_computation <- NULL
  }
  
  statistics <- dplyr::bind_cols(power_computation, 
                                 type_1_error_computation, 
                                 precision_computation) 
  
  select_columns <- rlang::syms(names(statistics)[names(statistics) %ni%
                       regmatches(names(statistics), 
                              regexpr(paste(paste0("^", 
                                                   group_vars, 
                                                   "[0-9]+"), 
                                            collapse = "|"), 
                                            names(statistics)))])
  
  statistics <- dplyr::select(statistics, !!! select_columns)
  
  statistics['replications'] <- sim_args['replications']
  
  statistics
  
}

compute_power <- function(data, sim_args) {
  
  power_args <- parse_power(sim_args)
  
  if(power_args$direction == 'lower') {
    data %>%
      mutate(reject = ifelse(statistic <= power_args['test_statistic'], 1, 0))
  } else {
    if(power_args$direction == 'upper') {
      data %>%
        mutate(reject = ifelse(statistic >= power_args['test_statistic'], 1, 0))
    } else {
      data %>%
        mutate(reject = ifelse(abs(statistic) >= power_args['test_statistic'], 1, 0))
    }
  }
}

compute_t1e <- function(data, sim_args) {
  
  t1e_args <- parse_power(sim_args)
  
  fixed_vars <- strsplit(as.character(parse_formula(sim_args)[['fixed']]), "\\+")[[2]]
  
  if(length(fixed_vars) == length(sim_args$reg_weights)) {
    reg_weights <- sim_args$reg_weights
  } else {
    reg_weights <- sim_args$model_fit$reg_weights
  }
  
  if(length(fixed_vars) != length(reg_weights)) {
    stop("Check reg_weights in model_fit simulation arguments, must specify 
         reg_weights if specifying model")
  }
  
  if(t1e_args$direction == 'lower') {
    data %>%
      mutate(adjusted_teststat = (estimate - reg_weights) / std.error,
             t1e = ifelse(adjusted_teststat <= t1e_args['test_statistic'], 
                          1, 0))
  } else {
    if(t1e_args$direction == 'upper') {
      data %>%
        mutate(adjusted_teststat = (estimate - reg_weights) / std.error,
               t1e = ifelse(adjusted_teststat >= t1e_args['test_statistic'], 
                            1, 0))
    } else {
      data %>%
        mutate(adjusted_teststat = (estimate - reg_weights) / std.error,
               t1e = ifelse(abs(adjusted_teststat) >= t1e_args['test_statistic'], 
                            1, 0))
    }
  }
}

aggregate_power <- function(data, group_var) {
  
  group_by_var <- dplyr::quos(!!! group_var)
  
  data %>%
    group_by(!!! group_by_var) %>%
    summarise(power = mean(reject),
              avg_test_stat = mean(statistic))
}

aggregate_t1e <- function(data, group_var) {
  
  group_by_var <- dplyr::quos(!!! group_var)
  
  data %>%
    group_by(!!! group_by_var) %>% 
    summarise(type_1_error = mean(t1e),
              avg_adjtest_stat = mean(adjusted_teststat))

}


aggregate_precision <- function(data, group_var) {
  
  group_by_var <- dplyr::quos(!!! group_var) 
  
  data %>%
    group_by(!!! group_by_var) %>% 
    summarise(param_estimate_sd = sd(estimate),
              avg_standard_error = mean(std.error),
              precision_ratio = param_estimate_sd / avg_standard_error)
}

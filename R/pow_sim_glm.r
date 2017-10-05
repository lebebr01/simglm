#' Power simulation for nested designs
#' 
#' Takes simulation conditions as input, exports power.
#' 
#' Power function to compute power for a regression term for the generalized 
#' linear mixed model. This function would need to be replicated to make any 
#' statement about power. Use \code{\link{sim_pow_glm}} as a convenient wrapper 
#' for this.
#' 
#' @seealso \code{\link{sim_pow_glm}} for a wrapper to replicate.
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
#' @param data_str Type of data. Must be "cross", "long", or "single".
#' @param cor_vars A vector of correlations between variables.
#' @param fact_vars A nested list of factor, categorical, or ordinal variable 
#'      specification, each list must include:
#'   \itemize{
#'        \item numlevels = Number of levels for ordinal or factor variables.
#'        \item var_type = Must be 'level1', 'level2', or 'level3'.
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
#' @param outcome_type A vector specifying the type of outcome, must be either
#'   logistic or poisson. Logitstic outcome will be 0/1 and poisson outcome will
#'   be counts.
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
#' @param missing TRUE/FALSE flag indicating whether missing data should be 
#'  simulated.
#' @param missing_args Additional missing arguments to pass to the missing_data 
#'  function. See \code{\link{missing_data}} for examples.
#' @param pow_param Name of variable to calculate power for, must be a name from
#'  fixed.
#' @param alpha What should the per test alpha rate be used for the hypothesis 
#'  testing.
#' @param pow_dist Which distribution should be used when testing hypothesis 
#'  test, z or t?
#' @param pow_tail One-tailed or two-tailed test?
#' @param lme4_fit_mod Valid lme4 formula syntax to be used for model fitting.
#' @param lme4_fit_family Valid lme4 family specification passed to glmer.
#' @param general_mod Valid model syntax. This syntax can be from any R package. 
#'   By default, broom is used to extract model result information. Note, 
#'   package must be defined or loaded prior to running the sim_pow function.
#' @param general_extract A valid function to extract model results if 
#'   general_mod argument is used. This argument is primarily used if extracting model
#'   results is not possibly using the broom package. If this is left NULL (default), 
#'   broom is used to collect model results.
#' @param ... Not currently used.
#' @export 
sim_pow_glm_nested3 <- function(fixed, random, random3, fixed_param, 
                                random_param = list(), random_param3 = list(), 
                                cov_param, k, n, p, data_str, cor_vars = NULL, 
                                fact_vars = list(NULL), 
                                unbal = list("level2" = FALSE, "level3" = FALSE), 
                                unbal_design = list("level2" = NULL, "level3" = NULL),
                                outcome_type, cross_class_params = NULL,
                                missing = FALSE, missing_args = list(NULL),
                                pow_param = NULL, alpha, pow_dist = c("z", "t"), 
                                pow_tail = c(1, 2), 
                                lme4_fit_mod = NULL, lme4_fit_family, 
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
  
  data <- sim_glm_nested3(fixed, random, random3, fixed_param, random_param, 
                               random_param3, cov_param, k, n, p, 
                               data_str, cor_vars, fact_vars, 
                               unbal, unbal_design, outcome_type = outcome_type, 
                               cross_class_params, ...)
  if(missing) {
    data <- do.call(missing_data, c(list(sim_data = data), 
                                         missing_args))
  }
  
  if(!is.null(lme4_fit_mod)) {
    if(!purrr::is_formula(lme4_fit_mod)) {
      stop('lme4_fit_mod must be a formula to pass to glmer')
    }
    temp_mod <- lme4::glmer(lme4_fit_mod, data = data, 
                            family = lme4_fit_family)
  } else {
    if(!is.null(general_mod)) {
      temp_mod <- eval(parse(text = general_mod))
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
      
      if(outcome_type == 'logistic') {
        temp_mod <- lme4::glmer(fm1, data = data, family = binomial)
      } else {
        temp_mod <- lme4::glmer(fm1, data = data, family = poisson)
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
  
  test_stat$reject <- ifelse(test_stat['estimate'] >= crit, 1, 0)
  
  test_stat
}

#' Power simulation for nested designs
#' 
#' Takes simulation conditions as input, exports power.
#' 
#' Power function to compute power for a regression term for the generalized 
#' linear mixed model. This function would need to be replicated to make any 
#' statement about power.  Use \code{\link{sim_pow_glm}} as a convenient wrapper 
#' for this.
#' 
#' @seealso \code{\link{sim_pow_glm}} for a wrapper to replicate.
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
#' @param outcome_type A vector specifying the type of outcome, must be either
#'   logistic or poisson. Logitstic outcome will be 0/1 and poisson outcome will
#'   be counts.
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
#' @param lme4_fit_mod Valid lme4 formula syntax to be used for model fitting.
#' @param lme4_fit_family Valid lme4 family specification passed to glmer.
#' @param general_mod Valid model syntax. This syntax can be from any R package. 
#'   By default, broom is used to extract model result information. Note, 
#'   package must be defined or loaded prior to running the sim_pow function.
#' @param general_extract A valid function to extract model results if 
#'   general_mod argument is used. This argument is primarily used if extracting model
#'   results is not possibly using the broom package. If this is left NULL (default), 
#'   broom is used to collect model results.
#' @param ... Not currently used.
#' @export 
sim_pow_glm_nested <- function(fixed, random, fixed_param, 
                          random_param = list(), cov_param, n, p, data_str, 
                          cor_vars = NULL, fact_vars = list(NULL),
                          unbal = list("level2" = FALSE, "level3" = FALSE), 
                          unbal_design = list("level2" = NULL, "level3" = NULL),
                          outcome_type, cross_class_params = NULL, missing = FALSE, 
                          missing_args = list(NULL), pow_param = NULL, 
                          alpha, pow_dist = c("z", "t"), pow_tail = c(1, 2), 
                          lme4_fit_mod = NULL, lme4_fit_family, 
                          general_mod = NULL, general_extract = NULL, ...) {
  
  fixed_vars <- attr(terms(fixed),"term.labels")    
  rand_vars <- attr(terms(random),"term.labels")
  if(length(rand_vars) == 0) {
    rand_vars <- 1
  }
  
  if(any(pow_param %ni% c(fixed_vars, '(Intercept)'))) { 
    stop('pow_param must be a subset of fixed')
  }

  data <- sim_glm_nested(fixed, random, fixed_param, random_param, 
                              cov_param, n, p, data_str, cor_vars, fact_vars, 
                              unbal, unbal_design, outcome_type = outcome_type, 
                              cross_class_params, ...)
  if(missing) {
    data <- do.call(missing_data, c(list(sim_data = data), 
                                         missing_args))
  }
  
  if(!is.null(lme4_fit_mod)) {
    if(!purrr::is_formula(lme4_fit_mod)) {
      stop('lme4_fit_mod must be a formula to pass to glmer')
    }
    temp_mod <- lme4::glmer(lme4_fit_mod, data = data, 
                            family = lme4_fit_family)
  } else {
    if(!is.null(general_mod)) {
      temp_mod <- eval(parse(text = general_mod))
    } else {
      fix1 <- paste("sim_data ~", paste(fixed_vars, collapse = "+"))
      if(missing) {
        fix1 <- gsub('sim_data', 'sim_data2', fix1)
      }
      ran1 <- paste("(", paste(rand_vars, collapse = "+"), "|clustID)", sep = "")
      fm1 <- as.formula(paste(fix1, ran1, sep = "+ "))
      
      if(outcome_type == 'logistic') {
        temp_mod <- lme4::glmer(fm1, data = data, family = binomial)
      } else 
      {
        temp_mod <- lme4::glmer(fm1, data = data, family = poisson)
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
  
  test_stat$reject <- ifelse(test_stat['estimate'] >= crit, 1, 0)
  
  test_stat
}


#' Function to simulate power.
#' 
#' Input simulation conditions and which term to compute power for, export 
#' reported power.
#' 
#' Power function to compute power for a regression term for simple generalized 
#' regression models. This function would need to be replicated to make any 
#' statement about power.  Use \code{\link{sim_pow_glm}} as a convenient wrapper
#' for this.
#' 
#' @seealso \code{\link{sim_pow_glm}} for a wrapper to replicate.
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
#' @param outcome_type A vector specifying the type of outcome, must be either
#'   logistic or poisson. Logitstic outcome will be 0/1 and poisson outcome will
#'   be counts.
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
#' @param glm_fit_mod Valid glm syntax to be used for model fitting.
#' @param glm_fit_family Valid family syntax to pass to the glm function.
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
sim_pow_glm_single <- function(fixed, fixed_param, cov_param, n, data_str, 
                           cor_vars = NULL, fact_vars = list(NULL),
                           outcome_type,
                           missing = FALSE, missing_args = list(NULL),
                           pow_param = NULL, alpha, pow_dist = c("z", "t"), 
                           pow_tail = c(1, 2), glm_fit_mod = NULL, 
                           glm_fit_family, general_mod = NULL, 
                           general_extract = NULL, ...) {
  
  fixed_vars <- attr(terms(fixed),"term.labels")
  
  if(any(pow_param %ni% c(fixed_vars, '(Intercept)', 'Intercept'))) { 
    stop('pow_param must be a subset of fixed')
  }
  
  data <- sim_glm_single(fixed, fixed_param, cov_param, n, data_str, 
                                cor_vars, fact_vars, outcome_type = outcome_type, 
                                ...)
  if(missing) {
    data <- do.call(missing_data, c(list(sim_data = data), 
                                    missing_args))
  }
  
  if(!is.null(glm_fit_mod)) {
    if(!purrr::is_formula(glm_fit_mod)) {
      stop('glm_fit_mod must be a formula to pass to glm')
    }
    temp_lm <- glm(glm_fit_mod, data = data, 
                   family = glm_fit_family)
  } else {
    if(!is.null(general_mod)) {
      temp_lm <- eval(parse(text = general_mod))
    } else {
      fm1 <- as.formula(paste("sim_data ~", paste(fixed_vars, collapse = "+")))
      if(missing) {
        fm1 <- as.formula(paste("sim_data2 ~", paste(fixed_vars, collapse = "+")))
      }
      
      if(outcome_type == 'logistic') {
        temp_lm <- glm(fm1, data = data, family = binomial)
      } else {
        temp_lm <- glm(fm1, data = data, family = poisson)
      }
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
  
  test_stat$reject <- ifelse(test_stat['estimate'] >= crit, 1, 0)
  
  test_stat
}

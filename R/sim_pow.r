#' Master power simulation function.
#' 
#' Input simulation conditions, returns power for term.
#' 
#' This function is a wrapper that replicates the simulation functions for 
#' simple regression and the linear mixed model power functions. This function 
#' replicates the power call a specified number of times and prints outs a 
#' matrix with the results.
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
#'   function. Required arguments include:
#'   \itemize{
#'     \item dist_fun: This is a quoted R distribution function.
#'     \item var_type: This is the level of variable to generate. Must be 
#'       either 'single', 'level1', 'level2', or 'level3'. 
#'       Must be same order as fixed formula above.
#'   }
#'   Optional arguments to the distribution functions are in a nested list,
#'    see the examples for example code for this.
#'  Does not include intercept, time, factors, or interactions.
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
#'        \item var_type = Must be 'single', 'level1', 'level2', or 'level3'.
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
#'  level one error generating function
#' @param arima_mod A list indicating the ARIMA model to pass to arima.sim. 
#'             See \code{\link{arima.sim}} for examples.
#' @param missing TRUE/FALSE flag indicating whether missing data should be 
#'  simulated.
#' @param missing_args Additional missing arguments to pass to the missing_data 
#'  function. See \code{\link{missing_data}} for examples.
#' @param pow_param Number of parameter to calculate power includes intercept 
#'  where applicable.
#' @param alpha What should the per test alpha rate be used for the hypothesis 
#'  testing.
#' @param pow_dist Which distribution should be used when testing hypothesis 
#'  test, z or t?
#' @param pow_tail One-tailed or two-tailed test?
#' @param replicates How many replications should be done (i.e. the denominator 
#'  in power calculation).
#' @param terms_vary A named list of terms that should vary as a function for 
#'  the power simulation. The names must match arguments to the simulation 
#'  function, see \code{\link{sim_reg}} for examples. Values specified here 
#'  should not be included as arguments in the function call.
#' @param raw_power TRUE/FALSE indicating whether raw power output should be 
#'  returned. Default is TRUE, which will create a new nested column with 
#'  raw data by variable(s) manipulated in power analysis.
#' @param lm_fit_mod Valid lm syntax to be used for model fitting.
#' @param lme4_fit_mod Valid lme4 syntax to be used for model fitting.
#' @param nlme_fit_mod Valid nlme syntax to be used for model fitting. 
#'   This should be specified as a named list with fixed and random components.
#' @param arima_fit_mod Valid nlme syntax for fitting serial correlation structures.
#'   See \code{\link{corStruct}} for help. This must be specified to 
#'   include serial correlation.
#' @param ... Currently not used.
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr '%>%'
#' @importFrom dplyr left_join
#' @importFrom tidyr nest
#' @export 
sim_pow <- function(fixed, random = NULL, random3 = NULL, fixed_param, 
                    random_param = list(NULL), random_param3 = list(NULL), 
                    cov_param, k = NULL, n, p = NULL, 
                    error_var, with_err_gen, arima = FALSE,
                    data_str, cor_vars = NULL, fact_vars = list(NULL), 
                    unbal = list("level2" = FALSE, "level3" = FALSE), 
                    unbal_design = list("level2" = NULL, "level3" = NULL),
                    lvl1_err_params = NULL, arima_mod = list(NULL),
                    missing = FALSE, missing_args = list(NULL),
                   pow_param, alpha, pow_dist = c("z", "t"), pow_tail = c(1, 2), 
                    replicates, terms_vary = NULL, raw_power = TRUE, 
                   lm_fit_mod = NULL, lme4_fit_mod = NULL, nlme_fit_mod = NULL,
                   arima_fit_mod = NULL, ...) {
  
  args <- list(fixed = fixed, random = random, random3 = random3,
               fixed_param = fixed_param, random_param = random_param,
               random_param3 = random_param3,
               cov_param = cov_param, k = k, n = n, p = p, 
               error_var = error_var, with_err_gen = with_err_gen, 
               arima = arima, data_str = data_str, cor_vars = cor_vars, 
               fact_vars = fact_vars, unbal = unbal,
               unbal_design = unbal_design,
               lvl1_err_params = lvl1_err_params,
               arima_mod = arima_mod, missing = missing, 
               missing_args = missing_args, pow_param = pow_param, 
               alpha = alpha, pow_dist = pow_dist, pow_tail = pow_tail, 
               lm_fit_mod = lm_fit_mod, lme4_fit_mod = lme4_fit_mod, 
               nlme_fit_mod = nlme_fit_mod, arima_fit_mod = arima_fit_mod)
  
  if(!is.null(terms_vary)) {
    args[names(terms_vary)] <- NULL
    conds <- expand.grid(terms_vary, KEEP.OUT.ATTRS = FALSE)
    if(any(sapply(conds, is.list))) {
      loc <- sapply(conds, is.list)
      simp_conds <- conds[loc != TRUE]
      list_conds <- conds[loc == TRUE]
      list_conds <- lapply(seq_along(list_conds), function(xx) 
        unlist(list_conds[xx], recursive = FALSE))
      for(tt in seq_along(list_conds)) {
        names(list_conds[[tt]]) <- gsub("[0-9]*", "", names(list_conds[[tt]]))
      }
      args <- lapply(1:nrow(conds), function(xx) c(args, 
                            simp_conds[xx, , drop = FALSE], 
                  do.call('c', lapply(seq_along(list_conds), function(tt) 
                    list_conds[[tt]][xx]))
                            ))
    } else {
      args <- lapply(1:nrow(conds), function(xx) c(args, 
                                        conds[xx, , drop = FALSE]))
    }
  }
  
  if(data_str == "single"){
    if(is.null(terms_vary)) {
      temp_pow <- do.call("rbind", lapply(seq_len(replicates), function(xx) 
          cbind(id = xx, do.call('sim_pow_single', args))
        ))
    } else {
      if(any(sapply(conds, is.list))) {
        temp_pow <- do.call('rbind', lapply(seq_along(args), function(tt)
          do.call("rbind", lapply(seq_len(replicates), function(xx) 
            cbind(rep = xx, do.call('sim_pow_single', args[[tt]]), 
                  simp_conds[tt, , drop = FALSE], 
                  lapply(seq_along(list_conds), function(xx) 
                    lapply(list_conds[[xx]], paste0, collapse = ',')[tt]), 
                  row.names = NULL)
          ))))
      } else {
        temp_pow <- do.call('rbind', lapply(seq_along(args), function(tt)
          do.call("rbind", lapply(seq_len(replicates), function(xx) 
            cbind(do.call('sim_pow_single', args[[tt]]), 
                  conds[tt, , drop = FALSE], row.names = NULL)
          ))))
      }
    }
  } else {
    if(is.null(k)) {
      if(is.null(terms_vary)) {
        temp_pow <- do.call("rbind", lapply(seq_len(replicates), function(xx) 
            do.call('sim_pow_nested', args)
          ))
      } else {
        if(any(sapply(conds, is.list))) {
          temp_pow <- do.call('rbind', lapply(seq_along(args), function(tt)
            do.call("rbind", lapply(seq_len(replicates), function(xx) 
              cbind(rep = xx, do.call('sim_pow_nested', args[[tt]]), 
                    simp_conds[tt, , drop = FALSE], 
                    lapply(seq_along(list_conds), function(xx) 
                      lapply(list_conds[[xx]], paste0, collapse = ',')[tt]),
                    row.names = NULL)
            ))))
        } else {
          temp_pow <- do.call('rbind', lapply(seq_along(args), function(tt)
            do.call("rbind", lapply(seq_len(replicates), function(xx) 
              cbind(do.call('sim_pow_nested', args[[tt]]), 
                    conds[tt, , drop = FALSE], row.names = NULL)
            ))))
        }
      }
    } else {
      if(is.null(terms_vary)) {
        temp_pow <- do.call("rbind", lapply(seq_len(replicates), function(xx) 
            do.call('sim_pow_nested3', args)
          ))
      } else {
        if(any(sapply(conds, is.list))) {
          temp_pow <- do.call('rbind', lapply(seq_along(args), function(tt)
            do.call("rbind", lapply(seq_len(replicates), function(xx) 
              cbind(rep = xx, do.call('sim_pow_nested3', args[[tt]]), 
                    simp_conds[tt, , drop = FALSE], 
                    lapply(seq_along(list_conds), function(xx) 
                      lapply(list_conds[[xx]], paste0, collapse = ',')[tt]),
                    row.names = NULL)
            ))))
        } else {
          temp_pow <- do.call('rbind', lapply(seq_along(args), function(tt)
            do.call("rbind", lapply(seq_len(replicates), function(xx) 
              cbind(do.call('sim_pow_nested3', args[[tt]]), 
                    conds[tt, , drop = FALSE], row.names = NULL)
            ))))
        }
      }
    }
  }
  
  grp_by <- lapply(c('var', names(terms_vary)), as.symbol)
  
  power <- temp_pow %>%
    dplyr::group_by_(.dots = grp_by) %>%
    dplyr::summarise(avg_test_stat = mean(test_stat),
                     sd_test_stat = sd(test_stat),
                     power = mean(reject),
                     num_reject = sum(reject),
                     num_repl = replicates)
  
  if(raw_power) {
    nest_power <- temp_pow %>%
      dplyr::group_by_(.dots = grp_by) %>%
      tidyr::nest()
    
    power <- left_join(power, nest_power, by = paste(grp_by, sep = ','))
  }
  
  power
}

#' Master power simulation function for glm models.
#' 
#' Input simulation conditions, returns power for term.
#' 
#' This function is a wrapper that replicates the simulation functions for 
#' simple generalized regression and the generalized linear mixed model power 
#' functions. This function replicates the power call a specified number of 
#' times and prints outs a matrix with the results.
#' 
#' @param fixed One sided formula for fixed effects in the simulation. 
#'  To suppress intercept add -1 to formula.
#' @param random One sided formula for random effects in the simulation. 
#'  Must be a subset of fixed.
#' @param random3 One sided formula for random effects at third level in the 
#'  simulation. Must be a subset of fixed(and likely of random).
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
#'   function. Required arguments include:
#'   \itemize{
#'     \item dist_fun: This is a quoted R distribution function.
#'     \item var_type: This is the level of variable to generate. Must be 
#'       either 'single', 'level1', 'level2', or 'level3'. 
#'       Must be same order as fixed formula above.
#'   }
#'   Optional arguments to the distribution functions are in a nested list,
#'    see the examples for example code for this.
#'  Does not include intercept, time, factors, or interactions.
#' @param k Number of third level clusters.
#' @param n Cluster sample size.
#' @param p Within cluster sample size.
#' @param data_str Type of data. Must be "cross", "long", or "single".
#' @param cor_vars A vector of correlations between variables.
#' @param fact_vars A nested list of factor, categorical, or ordinal variable 
#'      specification, each list must include:
#'   \itemize{
#'        \item numlevels = Number of levels for ordinal or factor variables.
#'        \item var_type = Must be 'single', 'level1', 'level2', or 'level3'.
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
#' @param missing TRUE/FALSE flag indicating whether missing data should be 
#'  simulated.
#' @param missing_args Additional missing arguments to pass to the missing_data 
#'  function. See \code{\link{missing_data}} for examples.
#' @param pow_param Number of parameter to calculate power includes intercept 
#'  where applicable.
#' @param alpha What should the per test alpha rate be used for the hypothesis 
#'  testing.
#' @param pow_dist Which distribution should be used when testing hypothesis 
#'  test, z or t?
#' @param pow_tail One-tailed or two-tailed test?
#' @param replicates How many replications should be done (i.e. the denominator 
#'  in power calculation).
#' @param terms_vary A named list of terms that should vary as a function for 
#'  the power simulation. The names must match arguments to the simulation 
#'  function, see \code{\link{sim_glm}} for examples. Values specified here 
#'  should not be included as arguments in the function call.
#' @param raw_power TRUE/FALSE indicating whether raw power output should be 
#'  returned. Default is TRUE, which will create a new nested column with 
#'  raw data by variable(s) manipulated in power analysis.
#' @param glm_fit_mod Valid glm syntax to be used for model fitting.
#' @param lme4_fit_mod Valid lme4 syntax to be used for model fitting.
#' @param glm_fit_family Valid family syntax to pass to the glm function.
#' @param lme4_fit_family Valid lme4 family specification passed to glmer.
#' @param ... Current not used.
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr '%>%'
#' @export 
sim_pow_glm <- function(fixed, random = NULL, random3 = NULL, fixed_param, 
                    random_param = list(NULL), random_param3 = list(NULL), 
                    cov_param, k = NULL, n, p = NULL, 
                    data_str, cor_vars = NULL, fact_vars = list(NULL), 
                    unbal = list("level2" = FALSE, "level3" = FALSE), 
                    unbal_design = list("level2" = NULL, "level3" = NULL),
                    missing = FALSE, missing_args = list(NULL),
                  pow_param, alpha, pow_dist = c("z", "t"), pow_tail = c(1, 2), 
                    replicates, terms_vary = NULL, raw_power = TRUE, 
                  glm_fit_mod = NULL, lme4_fit_mod = NULL, 
                  glm_fit_family = NULL, lme4_fit_family = NULL, ...) {
  
  args <- list(fixed = fixed, random = random, random3 = random3,
               fixed_param = fixed_param, random_param = random_param,
               random_param3 = random_param3,
               cov_param = cov_param, k = k, n = n, p = p, 
               data_str = data_str, cor_vars = cor_vars, 
               fact_vars = fact_vars, unbal = unbal, unbal_design = unbal_design,
               missing = missing, missing_args = missing_args,
               pow_param = pow_param, alpha = alpha, pow_dist = pow_dist, 
               pow_tail = pow_tail, glm_fit_mod = glm_fit_mod, 
               lme4_fit_mod = lme4_fit_mod, glm_fit_family = glm_fit_family,
               lme4_fit_family = lme4_fit_family)
  
  if(!is.null(terms_vary)) {
    args[names(terms_vary)] <- NULL
    conds <- expand.grid(terms_vary, KEEP.OUT.ATTRS = FALSE)
    if(any(sapply(conds, is.list))) {
      loc <- sapply(conds, is.list)
      simp_conds <- conds[loc != TRUE]
      list_conds <- conds[loc == TRUE]
      list_conds <- lapply(seq_along(list_conds), function(xx) 
        unlist(list_conds[xx], recursive = FALSE))
      for(tt in seq_along(list_conds)) {
        names(list_conds[[tt]]) <- gsub("[0-9]*", "", names(list_conds[[tt]]))
      }
      args <- lapply(1:nrow(conds), function(xx) c(args, 
                 simp_conds[xx, , drop = FALSE], 
                   do.call('c', lapply(seq_along(list_conds), function(tt) 
                                           list_conds[[tt]][xx]))
      ))
    } else {
      args <- lapply(1:nrow(conds), function(xx) c(args, 
                                                   conds[xx, , drop = FALSE]))
    }
  }
  
  if(data_str == "single"){
    if(is.null(terms_vary)) {
      temp_pow <- do.call("rbind", lapply(seq_len(replicates), function(xx) 
        do.call('sim_pow_glm_single', args)
      ))
    } else {
      if(any(sapply(conds, is.list))) {
        temp_pow <- do.call('rbind', lapply(seq_along(args), function(tt)
          do.call("rbind", lapply(seq_len(replicates), function(xx) 
            cbind(rep = xx, do.call('sim_pow_glm_single', args[[tt]]), 
                  simp_conds[tt, , drop = FALSE], 
                  lapply(seq_along(list_conds), function(xx) 
                    lapply(list_conds[[xx]], paste0, collapse = ',')[tt]), 
                  row.names = NULL)
          ))))
      } else {
        temp_pow <- do.call('rbind', lapply(seq_along(args), function(tt)
          do.call("rbind", lapply(seq_len(replicates), function(xx) 
            cbind(do.call('sim_pow_glm_single', args[[tt]]), 
                  conds[tt, , drop = FALSE], row.names = NULL)
          ))))
      }
    }
  } else {
    if(is.null(k)) {
      if(is.null(terms_vary)) {
        temp_pow <- do.call("rbind", lapply(seq_len(replicates), function(xx) 
          do.call('sim_pow_glm_nested', args)
        ))
      } else {
        if(any(sapply(conds, is.list))) {
          temp_pow <- do.call('rbind', lapply(seq_along(args), function(tt)
            do.call("rbind", lapply(seq_len(replicates), function(xx) 
              cbind(rep = xx, do.call('sim_pow_glm_nested', args[[tt]]), 
                    simp_conds[tt, , drop = FALSE], 
                    lapply(seq_along(list_conds), function(xx) 
                      lapply(list_conds[[xx]], paste0, collapse = ',')[tt]), 
                    row.names = NULL)
            ))))
        } else {
          temp_pow <- do.call('rbind', lapply(seq_along(args), function(tt)
            do.call("rbind", lapply(seq_len(replicates), function(xx) 
              cbind(do.call('sim_pow_glm_nested', args[[tt]]), 
                    conds[tt, , drop = FALSE], row.names = NULL)
            ))))
        }
      }
    } else {
      if(is.null(terms_vary)) {
        temp_pow <- do.call("rbind", lapply(seq_len(replicates), function(xx) 
          do.call('sim_pow_glm_nested3', args)
        ))
      } else {
        if(any(sapply(conds, is.list))) {
          temp_pow <- do.call('rbind', lapply(seq_along(args), function(tt)
            do.call("rbind", lapply(seq_len(replicates), function(xx) 
              cbind(rep = xx, do.call('sim_pow_glm_nested3', args[[tt]]), 
                    simp_conds[tt, , drop = FALSE], 
                    lapply(seq_along(list_conds), function(xx) 
                      lapply(list_conds[[xx]], paste0, collapse = ',')[tt]), 
                    row.names = NULL)
            ))))
        } else {
          temp_pow <- do.call('rbind', lapply(seq_along(args), function(tt)
            do.call("rbind", lapply(seq_len(replicates), function(xx) 
              cbind(do.call('sim_pow_glm_nested3', args[[tt]]), 
                    conds[tt, , drop = FALSE], row.names = NULL)
            ))))
        }
      }
    }
  }
  
  grp_by <- lapply(c('var', names(terms_vary)), as.symbol)
  
  power <- temp_pow %>%
    dplyr::group_by_(.dots = grp_by) %>%
    dplyr::summarise(avg_test_stat = mean(test_stat),
                     sd_test_stat = sd(test_stat),
                     power = mean(reject),
                     num_reject = sum(reject),
                     num_repl = replicates)
  
  if(raw_power) {
    nest_power <- temp_pow %>%
      dplyr::group_by_(.dots = grp_by) %>%
      tidyr::nest()
    
    power <- left_join(power, nest_power, by = paste(grp_by, sep = ','))
  }
  
  power
}

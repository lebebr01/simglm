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
#' @param fixed_param Fixed effect parameter values (i.e. beta weights).  Must be same length as fixed.
#' @param random_param A list of named elements that must contain: 
#'             random_var = variance of random parameters,
#'             rand_gen = Name of simulation function for random effects.
#'          Optional elements are:
#'             ther: Theorectial mean and variance from rand_gen,
#'             ther_sim: Simulate mean/variance for standardization purposes,
#'             cor_vars: Correlation between random effects,
#'             ...: Additional parameters needed for rand_gen function.
#' @param random_param3 A list of named elements that must contain: 
#'             random_var = variance of random parameters,
#'             rand_gen = Name of simulation function for random effects.
#'          Optional elements are:
#'             ther: Theorectial mean and variance from rand_gen,
#'             ther_sim: Simulate mean/variance for standardization purposes,
#'             cor_vars: Correlation between random effects,
#'             ...: Additional parameters needed for rand_gen function.
#' @param cov_param List of mean and variance for fixed effects. Does not include intercept, time, or 
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
#' @param fact_vars A nested list of factor, categorical, or ordinal variable specification, 
#'      each list must include numlevels and var_type (must be "lvl1" or "lvl2");
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
#' @param lvl1_err_params Additional parameters passed as a list on to the level one error generating function
#' @param arima_mod A list indicating the ARIMA model to pass to arima.sim. 
#'             See \code{\link{arima.sim}} for examples.
#' @param missing TRUE/FALSE flag indicating whether missing data should be simulated.
#' @param missing_args Additional missing arguments to pass to the missing_data function. 
#'           See \code{\link{missing_data}} for examples.
#' @param pow_param Number of parameter to calculate power includes intercept where applicable.
#' @param alpha What should the per test alpha rate be used for the hypothesis testing.
#' @param pow_dist Which distribution should be used when testing hypothesis test, z or t?
#' @param pow_tail One-tailed or two-tailed test?
#' @param replicates How many replications should be done (i.e. the denominator in power calculation).
#' @param terms_vary A named list of terms that should vary as a function for the power simulation.
#'          The names must match arguments to the simulation function, see \code{\link{sim_reg}} for 
#'          examples. Values specified here should not be included as arguments in the function call.
#' @param ... Currently not used.
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr '%>%'
#' @export 
sim_pow <- function(fixed, random = NULL, random3 = NULL, fixed_param, 
                    random_param = list(NULL), random_param3 = list(NULL), 
                    cov_param, k = NULL, n, p = NULL, 
                    error_var, with_err_gen, arima = FALSE,
                    data_str, cor_vars = NULL, fact_vars = list(NULL), unbal = FALSE, unbal3 = FALSE, 
                    unbalCont = NULL, unbalCont3 = NULL,
                    lvl1_err_params = NULL, arima_mod = list(NULL),
                    missing = FALSE, missing_args = list(NULL),
                    pow_param, alpha, pow_dist = c("z", "t"), pow_tail = c(1, 2), 
                    replicates, terms_vary = NULL, ...) {
  
  args <- list(fixed = fixed, random = random, random3 = random3,
               fixed_param = fixed_param, random_param = random_param,
               random_param3 = random_param3,
               cov_param = cov_param, k = k, n = n, p = p, 
               error_var = error_var, with_err_gen = with_err_gen, 
               arima = arima, data_str = data_str, cor_vars = cor_vars, 
               fact_vars = fact_vars, unbal = unbal, unbal3 = unbal3,
               unbalCont = unbalCont, unbalCont3 = unbalCont3,
               lvl1_err_params = lvl1_err_params,
               arima_mod = arima_mod, missing = missing, missing_args = missing_args,
               pow_param = pow_param, alpha = alpha, pow_dist = pow_dist, 
               pow_tail = pow_tail)
  if(!is.null(terms_vary)) {
    args[names(terms_vary)] <- NULL
    conds <- expand.grid(terms_vary, KEEP.OUT.ATTRS = FALSE)
    if(any(sapply(conds, is.list))) {
      loc <- sapply(conds, is.list)
      simp_conds <- conds[loc != TRUE]
      list_conds <- conds[loc == TRUE]
      list_conds <- unlist(list_conds, recursive = FALSE)
      names(list_conds) <- gsub("[0-9]*", "", names(list_conds))
      args <- lapply(1:nrow(conds), function(xx) c(args, 
                            simp_conds[xx, , drop = FALSE], list_conds[xx]))
    } else {
      args <- lapply(1:nrow(conds), function(xx) c(args, 
                                        conds[xx, , drop = FALSE]))
    }
  }
  
  if(data_str == "single"){
    if(is.null(terms_vary)) {
      temp_pow <- do.call("rbind", lapply(1:replicates, function(xx) 
          do.call('sim_pow_single', args)
        ))
    } else {
      if(any(sapply(conds, is.list))) {
        temp_pow <- do.call('rbind', lapply(seq_along(args), function(tt)
          do.call("rbind", lapply(1:replicates, function(xx) 
            cbind(rep = xx, do.call('sim_pow_single', args[[tt]]), 
                  simp_conds[tt, , drop = FALSE], 
                  lapply(list_conds, paste0, collapse = ',')[tt], 
                  row.names = NULL)
          ))))
      } else {
        temp_pow <- do.call('rbind', lapply(seq_along(args), function(tt)
          do.call("rbind", lapply(1:replicates, function(xx) 
            cbind(do.call('sim_pow_single', args[[tt]]), 
                  conds[tt, , drop = FALSE], row.names = NULL)
          ))))
      }
    }
  } else {
    if(is.null(k)) {
      if(is.null(terms_vary)) {
        temp_pow <- do.call("rbind", lapply(1:replicates, function(xx) 
            do.call('sim_pow_nested', args)
          ))
      } else {
        if(any(sapply(conds, is.list))) {
          temp_pow <- do.call('rbind', lapply(seq_along(args), function(tt)
            do.call("rbind", lapply(1:replicates, function(xx) 
              cbind(rep = xx, do.call('sim_pow_nested', args[[tt]]), 
                    simp_conds[tt, , drop = FALSE], row.names = NULL)
            ))))
        } else {
          temp_pow <- do.call('rbind', lapply(seq_along(args), function(tt)
            do.call("rbind", lapply(1:replicates, function(xx) 
              cbind(do.call('sim_pow_nested', args[[tt]]), 
                    conds[tt, , drop = FALSE], row.names = NULL)
            ))))
        }
      }
    } else {
      if(is.null(terms_vary)) {
        temp_pow <- do.call("rbind", lapply(1:replicates, function(xx) 
            do.call('sim_pow_nested3', args)
          ))
      } else {
        if(any(sapply(conds, is.list))) {
          temp_pow <- do.call('rbind', lapply(seq_along(args), function(tt)
            do.call("rbind", lapply(1:replicates, function(xx) 
              cbind(rep = xx, do.call('sim_pow_nested3', args[[tt]]), 
                    simp_conds[tt, , drop = FALSE], row.names = NULL)
            ))))
        } else {
          temp_pow <- do.call('rbind', lapply(seq_along(args), function(tt)
            do.call("rbind", lapply(1:replicates, function(xx) 
              cbind(do.call('sim_pow_nested3', args[[tt]]), 
                    conds[tt, , drop = FALSE], row.names = NULL)
            ))))
        }
      }
    }
  }
  if(is.null(terms_vary)) {
    power <- temp_pow %>%
      dplyr::group_by_('var') %>%
      dplyr::summarise(avg_test_stat = mean(test_stat),
                       sd_test_stat = sd(test_stat),
                       power = mean(reject),
                       num_reject = sum(reject),
                       num_repl = replicates)
  } else {
    grp_by <- lapply(c('var', names(terms_vary)), as.symbol)
    
    power <- temp_pow %>%
      dplyr::group_by_(.dots = grp_by) %>%
      dplyr::summarise(avg_test_stat = mean(test_stat),
                       sd_test_stat = sd(test_stat),
                       power = mean(reject),
                       num_reject = sum(reject),
                       num_repl = replicates)
  }
  return(power)
}

#' Master power simulation function for glm models.
#' 
#' Input simulation conditions, returns power for term.
#' 
#' This function is a wrapper that replicates the simulation functions for simple generalized regression
#' and the generalized linear mixed model power functions.  This function replicates the power call a 
#' specified number of times and prints outs a matrix with the results.
#' 
#' @param fixed One sided formula for fixed effects in the simulation.  To suppress intercept add -1 to formula.
#' @param random One sided formula for random effects in the simulation. Must be a subset of fixed.
#' @param random3 One sided formula for random effects at third level in the simulation. Must be a subset of fixed
#'  (and likely of random).
#' @param fixed_param Fixed effect parameter values (i.e. beta weights).  Must be same length as fixed.
#' @param random_param A list of named elements that must contain: 
#'             random_var = variance of random parameters,
#'             rand_gen = Name of simulation function for random effects.
#'          Optional elements are:
#'             ther: Theorectial mean and variance from rand_gen,
#'             ther_sim: Simulate mean/variance for standardization purposes,
#'             cor_vars: Correlation between random effects,
#'             ...: Additional parameters needed for rand_gen function.
#' @param random_param3 A list of named elements that must contain: 
#'             random_var = variance of random parameters,
#'             rand_gen = Name of simulation function for random effects.
#'          Optional elements are:
#'             ther: Theorectial mean and variance from rand_gen,
#'             ther_sim: Simulate mean/variance for standardization purposes,
#'             cor_vars: Correlation between random effects,
#'             ...: Additional parameters needed for rand_gen function.
#' @param cov_param List of mean and variance for fixed effects. Does not include intercept, time, or 
#' interactions. Must be same order as fixed formula above.
#' @param k Number of third level clusters.
#' @param n Cluster sample size.
#' @param p Within cluster sample size.
#' @param data_str Type of data. Must be "cross", "long", or "single".
#' @param cor_vars A vector of correlations between variables.
#' @param fact_vars A nested list of factor, categorical, or ordinal variable specification, 
#'      each list must include numlevels and var_type (must be "lvl1" or "lvl2");
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
#' @param missing TRUE/FALSE flag indicating whether missing data should be simulated.
#' @param missing_args Additional missing arguments to pass to the missing_data function. 
#'           See \code{\link{missing_data}} for examples.
#' @param pow_param Number of parameter to calculate power includes intercept where applicable.
#' @param alpha What should the per test alpha rate be used for the hypothesis testing.
#' @param pow_dist Which distribution should be used when testing hypothesis test, z or t?
#' @param pow_tail One-tailed or two-tailed test?
#' @param replicates How many replications should be done (i.e. the denominator in power calculation).
#' @param terms_vary A named list of terms that should vary as a function for the power simulation.
#'          The names must match arguments to the simulation function, see \code{\link{sim_glm}} for 
#'          examples. Values specified here should not be included as arguments in the function call.
#' @param ... Current not used.
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr '%>%'
#' @export 
sim_pow_glm <- function(fixed, random, random3, fixed_param, 
                    random_param = list(), random_param3 = list(), cov_param, k, n, p, 
                    data_str, cor_vars = NULL, fact_vars = list(NULL), unbal = FALSE, unbal3 = FALSE, 
                    unbalCont = NULL, unbalCont3 = NULL,
                    missing = FALSE, missing_args = list(NULL),
                    pow_param, alpha, pow_dist = c("z", "t"), pow_tail = c(1, 2), 
                    replicates, terms_vary = NULL, ...) {
  
  if(data_str == "single"){
    if(is.null(terms_vary)) {
      args <- list(fixed = fixed, fixed_param = fixed_param, cov_param = cov_param,
                   n = n, data_str = data_str, cor_vars = cor_vars, 
                   fact_vars = fact_vars, 
                   missing = missing, missing_args = missing_args,
                   pow_param = pow_param, alpha = alpha, pow_dist = pow_dist, 
                   pow_tail = pow_tail)
      temp_pow <- do.call("rbind", lapply(1:replicates, function(xx) 
        do.call('sim_pow_glm_single', args)
      ))
    } else {
      args <- list(fixed = fixed, fixed_param = fixed_param, cov_param = cov_param,
                   n = n, data_str = data_str, cor_vars = cor_vars, 
                   fact_vars = fact_vars, 
                   missing = missing, missing_args = missing_args,
                   pow_param = pow_param, alpha = alpha, pow_dist = pow_dist, 
                   pow_tail = pow_tail)
      args[names(terms_vary)] <- NULL
      conds <- expand.grid(terms_vary)
      args <- lapply(1:nrow(conds), function(xx) c(args, conds[xx, , drop = FALSE]))
      
      temp_pow <- do.call('rbind', lapply(seq_along(args), function(tt)
        do.call("rbind", lapply(1:replicates, function(xx) 
          cbind(do.call('sim_pow_glm_single', args[[tt]]), conds[tt, , drop = FALSE], row.names = NULL)
        ))))
    }
  } else {
    if(is.null(k)) {
      if(is.null(terms_vary)) {
        args <- list(fixed = fixed, random = random, 
                     fixed_param = fixed_param, random_param = random_param,
                     cov_param = cov_param, n = n, p = p, 
                     data_str = data_str, cor_vars = cor_vars, 
                     fact_vars = fact_vars, unbal = unbal, unbalCont = unbalCont,
                     missing = missing, missing_args = missing_args,
                     pow_param = pow_param, alpha = alpha, pow_dist = pow_dist, 
                     pow_tail = pow_tail)
        
        temp_pow <- do.call("rbind", lapply(1:replicates, function(xx) 
          do.call('sim_pow_glm_nested', args)
        ))
      } else {
        args <- list(fixed = fixed, random = random, 
                     fixed_param = fixed_param, random_param = random_param,
                     cov_param = cov_param, n = n, p = p, 
                     data_str = data_str, cor_vars = cor_vars, 
                     fact_vars = fact_vars, unbal = unbal, unbalCont = unbalCont,
                     missing = missing, missing_args = missing_args,
                     pow_param = pow_param, alpha = alpha, pow_dist = pow_dist, 
                     pow_tail = pow_tail)
        args[names(terms_vary)] <- NULL
        conds <- expand.grid(terms_vary)
        args <- lapply(1:nrow(conds), function(xx) c(args, conds[xx, , drop = FALSE]))
        
        temp_pow <- do.call('rbind', lapply(seq_along(args), function(tt)
          do.call("rbind", lapply(1:replicates, function(xx) 
            cbind(do.call('sim_pow_glm_nested', args[[tt]]), conds[tt, , drop = FALSE], row.names = NULL)
          ))))
      }
    } else {
      if(is.null(terms_vary)) {
        args <- list(fixed = fixed, random = random, random3 = random3,
                     fixed_param = fixed_param, random_param = random_param,
                     random_param3 = random_param3,
                     cov_param = cov_param, k = k, n = n, p = p, 
                     data_str = data_str, cor_vars = cor_vars, 
                     fact_vars = fact_vars, unbal = unbal, unbal3 = unbal3,
                     unbalCont = unbalCont, unbalCont3 = unbalCont3,
                     missing = missing, missing_args = missing_args,
                     pow_param = pow_param, alpha = alpha, pow_dist = pow_dist, 
                     pow_tail = pow_tail)
        
        temp_pow <- do.call("rbind", lapply(1:replicates, function(xx) 
          do.call('sim_pow_glm_nested3', args)
        ))
      } else {
        args <- list(fixed = fixed, random = random, random3 = random3,
                     fixed_param = fixed_param, random_param = random_param,
                     random_param3 = random_param3,
                     cov_param = cov_param, k = k, n = n, p = p, 
                     data_str = data_str, cor_vars = cor_vars, 
                     fact_vars = fact_vars, unbal = unbal, unbal3 = unbal3,
                     unbalCont = unbalCont, unbalCont3 = unbalCont3,
                     missing = missing, missing_args = missing_args,
                     pow_param = pow_param, alpha = alpha, pow_dist = pow_dist, 
                     pow_tail = pow_tail)
        args[names(terms_vary)] <- NULL
        conds <- expand.grid(terms_vary)
        args <- lapply(1:nrow(conds), function(xx) c(args, conds[xx, , drop = FALSE]))
        
        temp_pow <- do.call('rbind', lapply(seq_along(args), function(tt)
          do.call("rbind", lapply(1:replicates, function(xx) 
            cbind(do.call('sim_pow_glm_nested3', args[[tt]]), conds[tt, , drop = FALSE], row.names = NULL)
          ))))
      }
    }
  }
  
  if(is.null(terms_vary)) {
    power <- temp_pow %>%
      dplyr::group_by_('var') %>%
      dplyr::summarise(avg_test_stat = mean(test_stat),
                       sd_test_stat = sd(test_stat),
                       power = mean(reject),
                       num_reject = sum(reject),
                       num_repl = replicates)
  } else {
    grp_by <- lapply(c('var', names(terms_vary)), as.symbol)
    
    power <- temp_pow %>%
      dplyr::group_by_(.dots = grp_by) %>%
      dplyr::summarise(avg_test_stat = mean(test_stat),
                       sd_test_stat = sd(test_stat),
                       power = mean(reject),
                       num_reject = sum(reject),
                       num_repl = replicates)
  }
  return(power)
}

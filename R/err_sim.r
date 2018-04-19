#' Function that simulates errors.
#' 
#' Input error simulation parameters and outputs simulated errors.
#' 
#' @param error_var Scalar of error variance
#' @param n Cluster sample size.
#' @param p Within cluster sample size.
#' @param with_err_gen The generating function used as a character, 
#'             (e.g. 'rnorm').
#' @param arima TRUE/FALSE flag indicating whether residuals should 
#'             be correlated. If TRUE, must specify a valid model to pass to 
#'             arima.sim via the arima_mod argument. 
#'             See \code{\link{arima.sim}} for examples.
#' @param lvl1_err_params Additional values that need to be passed to the 
#'  function called from with_err_gen.
#' @param arima_mod A list indicating the ARIMA model to pass to arima.sim. 
#'             See \code{\link{arima.sim}} for examples.
#' @param ther A vector of length two that specifies the theoretical mean and 
#'          standard deviation of the with_err_gen. This would commonly be 
#'          used to standardize the generating variable to have a mean of 0 and
#'          standard deviation of 1 to meet model assumptions. The variable
#'          is then rescaled to have the variance specified by error_var.
#' @param ther_sim A TRUE/FALSE flag indicating whether the error simulation 
#'  function should be simulated, that is should the mean and standard deviation
#'  used for standardization be simulated.
#' @param homogeneity Either TRUE (default) indicating homogeneity of variance
#'  assumption is assumed or FALSE to indicate desire to generate heterogeneity 
#'  of variance.
#' @param fixef The design matrix, this is passed internally and used for 
#'  heterogeneity of variance simulation.
#' @param heterogeneity_var Variable name as a character string to use for 
#'  heterogeneity of variance simulation.
#' @param ... Not currently used.
#' @export 
sim_err_nested <- function(error_var, n, p, with_err_gen, arima = FALSE,
                           lvl1_err_params = NULL, arima_mod = list(NULL),
                           ther = c(0, 1), ther_sim = FALSE, 
                           homogeneity = TRUE, fixef = NULL, 
                           heterogeneity_var = NULL, ...){
  
  if(ther_sim) {
    ther_val <- do.call(with_err_gen, c(list(n = 10000000), lvl1_err_params))
    ther <- c(mean(ther_val), sd(ther_val))
  }
  
  if(homogeneity) {
    if(arima) {
      args <- c(list(model = arima_mod, 
                     rand.gen = eval(parse(text = with_err_gen))), 
                lvl1_err_params)
      err <- unlist(lapply(mapply(arima.sim, n = p,
                                  MoreArgs = args, SIMPLIFY = FALSE), 
                           standardize, mean = ther[1], sd = ther[2])) * 
        sqrt(error_var)
    } else {
      err <- unlist(lapply(mapply(with_err_gen, n = p, 
                                  MoreArgs = lvl1_err_params, SIMPLIFY = FALSE), 
                           standardize, mean = ther[1], sd = ther[2])) * sqrt(error_var)
    }
  } else {
    if(arima) {
      args <- c(list(model = arima_mod, 
                     rand.gen = eval(parse(text = with_err_gen))), 
                lvl1_err_params)
      err <- unlist(lapply(mapply(arima.sim, n = p,
                                  MoreArgs = args, SIMPLIFY = FALSE), 
                           standardize, mean = ther[1], sd = ther[2]))
      err <- heterogeneity(error_var, fixef = fixef,
                           heterogeneity_var,
                           err)
    } else {
      args <- c(list(n = p), lvl1_err_params)
      err <- unlist(lapply(mapply(with_err_gen, n = p, 
                                  MoreArgs = lvl1_err_params, SIMPLIFY = FALSE), 
                           standardize, mean = ther[1], sd = ther[2]))
      err <- heterogeneity(error_var, fixef = fixef,
                           heterogeneity_var,
                           err)
    }
  }
  err
}


#' Function that simulates errors.
#' 
#' Input error simulation parameters and outputs simulated errors.
#' 
#' Simulates error term for single level regression models.
#' 
#' @param error_var Numeric scalar of error variance or vector used when 
#'   simulating heterogeneity of variance.
#' @param n Cluster sample size.
#' @param with_err_gen The generating function used.
#' @param arima TRUE/FALSE flag indicating whether residuals should 
#'             be correlated. If TRUE, must specify a valid model to pass to 
#'             arima.sim via the arima_mod argument. 
#'             See \code{\link{arima.sim}} for examples.
#' @param lvl1_err_params Additional values that need to be passed to the 
#'  function called from with_err_gen.
#' @param arima_mod A list indicating the ARIMA model to pass to arima.sim. 
#'             See \code{\link{arima.sim}} for examples.
#' @param ther A vector of length two that specifies the theoretical mean and 
#'          standard deviation of the with_err_gen. This would commonly be used
#'          to standardize the generating variable to have a mean of 0 and
#'          standard deviation of 1 to meet model assumptions. The variable
#'          is then rescaled to have the variance specified by error_var.
#' @param ther_sim A TRUE/FALSE flag indicating whether the error simulation 
#'  function should be simulated, that is should the mean and standard deviation
#'  used for standardization be simulated.
#' @param homogeneity Either TRUE (default) indicating homogeneity of variance
#'  assumption is assumed or FALSE to indicate desire to generate heterogeneity 
#'  of variance.
#' @param fixef The design matrix, this is passed internally and used for 
#'  heterogeneity of variance simulation.
#' @param heterogeneity_var Variable name as a character string to use for 
#'  heterogeneity of variance simulation.
#' @param ... Not currently used.
#' @export 
sim_err_single <- function(error_var, n, with_err_gen, arima = FALSE, 
                           lvl1_err_params = NULL, arima_mod = list(NULL),
                           ther = c(0, 1), ther_sim = FALSE, 
                           homogeneity = TRUE, fixef = NULL,
                           heterogeneity_var = NULL,
                           ...){
  
  if(ther_sim) {
    ther_val <- do.call(with_err_gen, c(list(n = 10000000), lvl1_err_params))
    ther <- c(mean(ther_val), sd(ther_val))
  }
  
  if(homogeneity) {
    if(arima) {
      args <- c(list(model = arima_mod, n = n, 
                     rand.gen = eval(parse(text = with_err_gen))), 
                lvl1_err_params)
      err <- standardize(do.call(arima.sim, args), 
                         mean = ther[1], sd = ther[2]) * sqrt(error_var)
    } else {
      args <- c(list(n = n), lvl1_err_params)
      err <- standardize(do.call(with_err_gen, args), 
                         mean = ther[1], sd = ther[2]) * sqrt(error_var)
    }
  } else {
    if(arima) {
      args <- c(list(model = arima_mod, n = n,
                     rand.gen = eval(parse(text = with_err_gen))),
                lvl1_err_params)
      err <- standardize(do.call(arima.sim, args),
                         mean = ther[1], sd = ther[2])
      err <- heterogeneity(error_var, fixef = fixef,
                           heterogeneity_var,
                           err)
      
    } else {
      args <- c(list(n = n), lvl1_err_params)
      err <- standardize(do.call(with_err_gen, args),
                         mean = ther[1], sd = ther[2])
      err <- heterogeneity(error_var, fixef = fixef,
                           heterogeneity_var,
                           err)
    }
  }
  
  err
}

heterogeneity <- function(variance, fixef, variable, err) {
  
  fixef_h <- data.frame(r_num = as.numeric(rownames(fixef)), err = err)
  
  if(length(unique(fixef[, variable])) == length(variance)) {
    fixef_h <- cbind(fixef_h, h_var = fixef[, variable])
  } else {
    fixef_h <- cbind(fixef_h, h_var = cut(fixef[, variable], 
                                          length(variance), labels = FALSE))
  }
  
  list_dat <- split(fixef_h, fixef_h$h_var)
  
  l_dat <- lapply(seq_along(variance), function(xx) 
    list_dat[[xx]]['err'] * sqrt(variance[xx]))
  
  dat <- cbind(do.call('rbind', list_dat), do.call('rbind', l_dat))
  colnames(dat) <- c('r_num', 'err_old', 'h_var', 'err')
  dat <- dat[order(dat$r_num),]
  
  dat['err']
}

sim_error <- function(...) {
  
  sim_continuous2(...)
  
}

#' Tidy error simulation
#' 
#' @param data Data simulated from other functions to pass to this function.
#' @param sim_args A named list with special model formula syntax. See details and examples
#'   for more information. The named list may contain the following:
#'   \itemize{
#'     \item fixed: This is the fixed portion of the model (i.e. covariates)
#'     \item random: This is the random portion of the model (i.e. random effects)
#'     \item error: This is the error (i.e. residual term).
#'   }
#' @param ... Other arguments to pass to error simulation functions.
#' 
#' @export 
simulate_error <- function(data, sim_args, ...) {
  
  if(is.null(data)) {
    n <- sample_sizes(sim_args[['sample_size']])
    ids <- create_ids(n, 
                      c('level1_id', parse_randomeffect(parse_formula(sim_args)[['randomeffect']])[['cluster_id_vars']]))
    error <- purrr::invoke(sim_error, 
                           sim_args[['error']],
                           n = n
    ) %>% 
      unlist()
  } else {
    n <- compute_samplesize(data, sim_args)
    error <- purrr::invoke(sim_error, 
                           sim_args[['error']],
                           n = n
    ) %>% 
      unlist()
  }
  
  if(is.null(data)) {
    data.frame(error = error, ids)
  } else {
    data.frame(data, error = error)
  }
}


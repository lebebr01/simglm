#' Function that simulates errors.
#' 
#' Input error simulation parameters and outputs simulated errors.
#' 
#' @param error_var Scalar of error variance
#' @param n Cluster sample size.
#' @param p Within cluster sample size.
#' @param with_err_gen The generating function used.
#' @param arima TRUE/FALSE flag indicating whether residuals should 
#'             be correlated. If TRUE, must specify a valid model to pass to 
#'             arima.sim. See \code{\link{arima.sim}} for examples.
#' @param lvl1_err_params Additional values that need to be passed to the function
#'             called from with_err_gen.
#' @param arima_mod A list indicating the ARIMA model to pass to arima.sim. 
#'             See \code{\link{arima.sim}} for examples.
#' @param ther A vector of length two that specifies the theoretical mean and 
#'              standard deviation of the with_err_gen. This would commonly be used
#'              to standardize the generating variable to have a mean of 0 and
#'              standard deviation of 1 to meet model assumptions. The variable
#'              is then rescaled to have the variance specified by error_var.
#' @param ther_sim A TRUE/FALSE flag indicating whether the error simulation function
#'              should be simulated, that is should the mean and standard deviation
#'              used for standardization be simulated.
#' @param ... Not currently used.
#' @export 
sim_err_nested <- function(error_var, n, p, with_err_gen, arima = FALSE,
                           lvl1_err_params = NULL, arima_mod = list(NULL),
                           ther = c(0, 1), ther_sim = FALSE, ...){
  
  if(ther_sim) {
    ther_val <- do.call(with_err_gen, c(list(n = 10000000), lvl1_err_params))
    ther <- c(mean(ther_val), sd(ther_val))
  }
  
  if(arima) {
    args <- c(list(model = arima_mod, 
                   rand.gen = eval(parse(text = with_err_gen))), 
              lvl1_err_params)
    err <- unlist(lapply(mapply(arima.sim, n = p,
                                MoreArgs = args, SIMPLIFY = FALSE), 
                         standardize, mean = ther[1], sd = ther[2])) * sqrt(error_var)
  } else {
    args <- c(list(n = p), lvl1_err_params)
    err <- unlist(lapply(mapply(with_err_gen, n = p, 
         MoreArgs = lvl1_err_params, SIMPLIFY = FALSE), 
         standardize, mean = ther[1], sd = ther[2])) * sqrt(error_var)
  }
  return(err)
}


#' Function that simulates errors.
#' 
#' Input error simulation parameters and outputs simulated errors.
#' 
#' Simulates error term for single level regression models.
#' 
#' @param error_var Numeric scalar of error variance
#' @param n Cluster sample size.
#' @param with_err_gen The generating function used.
#' @param arima TRUE/FALSE flag indicating whether residuals should 
#'             be correlated. If TRUE, must specify a valid model to pass to 
#'             arima.sim, See \code{\link{arima.sim}} for examples.
#' @param lvl1_err_params Additional values that need to be passed to the function
#'             called from with_err_gen.
#' @param arima_mod A list indicating the ARIMA model to pass to arima.sim. 
#'             See \code{\link{arima.sim}} for examples.
#' @param ther A vector of length two that specifies the theoretical mean and 
#'              standard deviation of the with_err_gen. This would commonly be used
#'              to standardize the generating variable to have a mean of 0 and
#'              standard deviation of 1 to meet model assumptions. The variable
#'              is then rescaled to have the variance specified by error_var.
#' @param ther_sim A TRUE/FALSE flag indicating whether the error simulation function
#'              should be simulated, that is should the mean and standard deviation
#'              used for standardization be simulated.
#' @param ... Not currently used.
#' @export 
sim_err_single <- function(error_var, n, with_err_gen, arima = FALSE, 
                           lvl1_err_params = NULL, arima_mod = list(NULL),
                           ther = c(0, 1), ther_sim = FALSE, 
                           ...){
  
  if(ther_sim) {
    ther_val <- do.call(with_err_gen, c(list(n = 10000000), lvl1_err_params))
    ther <- c(mean(ther_val), sd(ther_val))
  }

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
  return(err)
}

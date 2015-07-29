#' Function that simulates errors.
#' 
#' Input error simulation parameters and outputs simulated errors.
#' 
#' @param error_var Scalar of error variance
#' @param n Cluster sample size.
#' @param p Within cluster sample size.
#' @param rand_gen The generating function used.
#' @param arima TRUE/FALSE flag indicating whether residuals should 
#'             be correlated. If TRUE, must specify a valid model to pass to 
#'             arima.sim. See \code{\link{arima.sim}} for examples.
#' @param ther A vector of length two that specifies the theoretical mean and 
#'              standard deviation of the rand_gen. This would commonly be used
#'              to standardize the generating variable to have a mean of 0 and
#'              standard deviation of 1 to meet model assumptions. The variable
#'              is then rescaled to have the variance specified by error_var.
#' @param ther_sim A TRUE/FALSE flag indicating whether the error simulation function
#'              should be simulated, that is should the mean and standard deviation
#'              used for standardization be simulated.
#' @param ... Additional specification needed to pass to the random generating 
#'             function defined by rand.gen.
#' @export 
sim_err_nested <- function(error_var, n, p, rand_gen, arima = FALSE,
                           ther = c(0, 1), ther_sim = FALSE,...){
  
  # Look to edit this with match.arg and switch
  #n <- length(p)
  
  if(ther_sim) {
    ther_val <- sapply(X = 1000000, FUN = rand_gen, ...)
    ther <- c(mean(ther_val), sd(ther_val))
  }

  if(arima) {
    err <- unlist(lapply(1:n, function(xx) lapply( 
      arima.sim(n = p[xx], rand.gen = rand_gen, ...), 
      standardize, mean = ther[1], sd = ther[2]))) * sqrt(error_var)
  } else {
    err <- unlist(lapply(1:n, function(xx) lapply(mapply(rand_gen, n = p, ..., SIMPLIFY = FALSE), 
                         standardize, mean = ther[1], sd = ther[2]))) * sqrt(error_var)
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
#' @param rand_gen The generating function used.
#' @param arima TRUE/FALSE flag indicating whether residuals should 
#'             be correlated. If TRUE, must specify a valid model to pass to 
#'             arima.sim, See \code{\link{arima.sim}} for examples.
#' @param ther A vector of length two that specifies the theoretical mean and 
#'              standard deviation of the rand_gen. This would commonly be used
#'              to standardize the generating variable to have a mean of 0 and
#'              standard deviation of 1 to meet model assumptions. The variable
#'              is then rescaled to have the variance specified by error_var.
#' @param ther_sim A TRUE/FALSE flag indicating whether the error simulation function
#'              should be simulated, that is should the mean and standard deviation
#'              used for standardization be simulated.
#' @param ... Additional values that need to be passed to the function
#'             called from rand_gen.
#' @export 
sim_err_single <- function(error_var, n, rand_gen, arima = FALSE, 
                           ther = c(0, 1), ther_sim = FALSE, ...){
  
  if(ther_sim) {
    ther_val <- sapply(X = 1000000, FUN = rand_gen, ...)
    ther <- c(mean(ther_val), sd(ther_val))
  }

  if(arima) {
    err <- standardize(arima.sim(n = n, rand.gen = rand_gen, ...), 
                       mean = ther[1], sd = ther[2]) * sqrt(error_var)
  } else {
    err <- standardize(sapply(X = n, FUN = rand_gen, ...), 
                       mean = ther[1], sd = ther[2]) * sqrt(error_var)
  }
  return(err)
}

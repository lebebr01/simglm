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
#'             arima.sim, \seealso{arima.sim}
#' @param ... Additional specification needed to pass to the random generating 
#'             function defined by rand.gen.
#' @export 
sim_err_nested <- function(error_var, n, p, rand_gen, arima = FALSE, ...){
  
  # Look to edit this with match.arg and switch
  #n <- length(p)

  if(arima) {
    err <- unlist(lapply(lapply(1:n, function(xx) 
      arima.sim(n = p, rand.gen = rand_gen, ...)), scale)) * sqrt(error_var)
  } else {
    err <- unlist(lapply(lapply(1:n, rand_gen, n = p, ...), scale)) * sqrt(error_var)
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
#'             arima.sim, \seealso{arima.sim}.
#' @param ... Additional values that need to be passed to the function
#'             called from rand.gen.
#' @importFrom VGAM rlaplace
#' @export 
sim_err_single <- function(error_var, n, rand_gen, arima = FALSE, ...){
  
  if(arima) {
    err <- scale(arima.sim(n = n, rand.gen = rand_gen, ...)) * sqrt(error_var)
  } else {
    err <- scale(sapply(X = n, FUN = rand_gen, ...)) * sqrt(error_var)
  }
  return(err)
}

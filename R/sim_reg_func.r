#' Master function to simulate single level data.
#' 
#' Takes simulation parameters as inputs and returns simulated data.
#' 
#' Simulates data for the simple regression models.  Returns a data frame with 
#' ID variables, fixed effects, and many other variables useful to help when 
#' running simulation studies.
#' 
#' @seealso \code{\link{sim_reg}} for a convenient wrapper for all data 
#' conditions.
#' 
#' @param fixed One sided formula for fixed effects in the simulation.  
#'   To suppress intercept add -1 to formula.
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
#' @param with_err_gen Simulated within cluster error distribution. 
#'   Must be a quoted 'r' distribution function.
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
#'        \item var_type = Must be 'single'.
#'    }
#'    Optional arguments include:
#'    \itemize{
#'        \item replace
#'        \item prob
#'        \item value.labels
#'    }
#'     See also \code{\link{sample}} for use of these optional arguments.
#' @param lvl1_err_params Additional parameters passed as a list on to the 
#'  level one error generating function
#' @param arima_mod A list indicating the ARIMA model to pass to arima.sim. 
#'             See \code{\link{arima.sim}} for examples.
#' @param contrasts An optional list that specifies the contrasts to be used 
#'  for factor variables (i.e. those variables with .f or .c). 
#'  See \code{\link{contrasts}} for more detail.
#' @param ... Not currently used.
#' @export 
#' @examples 
#' #' # generating parameters for single level regression
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
#'    
sim_reg_single <- function(fixed, fixed_param, cov_param, n, error_var, 
                           with_err_gen, arima = FALSE, data_str, 
                           cor_vars = NULL, fact_vars = list(NULL), 
                           lvl1_err_params = NULL, arima_mod = list(NULL), 
                           contrasts = NULL, ...) {
  
  fixed_vars <- attr(terms(fixed),"term.labels")   

  Xmat <- sim_fixef_single(fixed = fixed, fixed_vars = fixed_vars, n = n, 
                           cov_param = cov_param, cor_vars = cor_vars, 
                           fact_vars = fact_vars, contrasts = contrasts)
  
  if(ncol(Xmat) != length(fixed_param)) {
    stop(paste(length(fixed_param), 'parameters specified for', ncol(Xmat), 
               'variables in design matrix'))
  }
  
  err <- sim_err_single(error_var, n, with_err_gen, arima = arima, 
                        lvl1_err_params = lvl1_err_params, 
                        arima_mod = arima_mod)
  
  sim_data <- data_reg_single(Xmat, fixed_param, n, err)
  
  Xmat <- data.frame(Xmat,sim_data)
  Xmat$ID <- 1:n
  
  Xmat
}


#' Function to simulate nested data
#' 
#' Takes simulation parameters as inputs and returns simulated data.
#' 
#' Simulates data for the linear mixed model, both cross sectional and 
#' longitudinal data. Returns a data frame with ID variables, fixed effects, 
#' and many other variables useful to help when running simulation studies.
#' 
#' @seealso \code{\link{sim_reg}} for a convenient wrapper for all data 
#'  conditions.
#' 
#' @param fixed One sided formula for fixed effects in the simulation. 
#'   To suppress intercept add -1 to formula.
#' @param random One sided formula for random effects in the simulation. 
#'   Must be a subset of fixed.
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
#' 
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
#'        \item var_type = Must be 'lvl1' or 'lvl2'.
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
#' @param lvl1_err_params Additional parameters passed as a list on to the 
#'  level one error generating function
#' @param arima_mod A list indicating the ARIMA model to pass to arima.sim. 
#'             See \code{\link{arima.sim}} for examples.
#' @param contrasts An optional list that specifies the contrasts to be used 
#'      for factor variables (i.e. those variables with .f or .c). 
#'      See \code{\link{contrasts}} for more detail.
#' @param ... Not currently used.
#' @export 
#' @examples 
#' #' # Longitudinal linear mixed model example
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
sim_reg_nested <- function(fixed, random, fixed_param, random_param = list(), 
                           cov_param, n, p, error_var, with_err_gen, 
                           arima = FALSE, data_str, cor_vars = NULL, 
                           fact_vars = list(NULL), unbal = FALSE, 
                           unbalCont = NULL, lvl1_err_params = NULL, 
                           arima_mod = list(NULL), contrasts = NULL, ...) {

  fixed_vars <- attr(terms(fixed),"term.labels")  
  rand.vars <- attr(terms(random),"term.labels")  

  if(unbal == FALSE) {
    lvl1ss <- rep(p, n)
    if(is.null(lvl1ss)) stop("lvl1ss is NULL")
  } else {
    if(length(unbalCont) < 2) stop("Must specify unbalCont when unbal = TRUE")
    lvl1ss <- round(runif(n = n, min = min(unbalCont), max = max(unbalCont)), 0)
  }

  rand_eff <- do.call(sim_rand_eff, c(random_param, n = n))

  Xmat <- sim_fixef_nested(fixed = fixed, fixed_vars = fixed_vars, 
                           cov_param = cov_param, n = n, p = lvl1ss, 
                            data_str = data_str, cor_vars = cor_vars, 
                           fact_vars = fact_vars, contrasts = contrasts)
  
  if(ncol(Xmat) != length(fixed_param)) {
    stop(paste(length(fixed_param), 'parameters specified for', ncol(Xmat), 
               'variables in design matrix'))
  }
  
  reff <- do.call("cbind", lapply(1:ncol(rand_eff), function(xx) 
    rep(rand_eff[,xx], times = lvl1ss)))
  colnames(reff) <- c(unlist(lapply(1:ncol(rand_eff), function(xx) 
    paste("b", xx-1, sep = ""))))
  
  Zmat <- model.matrix(random, data.frame(Xmat))

  err <- sim_err_nested(error_var, n, p = lvl1ss, with_err_gen = with_err_gen,
                        arima = arima, lvl1_err_params = lvl1_err_params, 
                        arima_mod = arima_mod, ...)

 sim_data <- data_reg_nested(Xmat, Zmat, fixed_param, rand_eff, n, 
                             p = lvl1ss, err = err)
  
 Xmat <- data.frame(Xmat,reff,sim_data)
 Xmat$withinID <- unlist(lapply(1:length(lvl1ss), function(xx) 1:lvl1ss[xx]))
 Xmat$clustID <- rep(1:n, times = lvl1ss)
 
 Xmat
}

#' Function to simulate three level nested data
#' 
#' Takes simulation parameters as inputs and returns simulated data.
#' 
#' Simulates data for the linear mixed model, both cross sectional and 
#' longitudinal data. Returns a data frame with ID variables, fixed effects, 
#' and many other variables useful to help when running simulation studies.
#' 
#' @seealso \code{\link{sim_reg}} for a convenient wrapper for all data 
#'  conditions.
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
#' @param n Level two cluster sample size within each level three cluster.
#' @param p Within cluster sample size within each level two cluster.
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
#'        \item var_type = Must be 'lvl1', 'lvl2', or 'lvl3'.
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
#'  level one size,will be drawn from a random uniform distribution with min 
#'  and max specified.
#' @param unbalCont3 When unbal3 = TRUE, this specifies the minimum and maximum 
#'  level two size, will be drawn from a random uniform distribution with min 
#'  and max specified.
#' @param lvl1_err_params Additional parameters passed as a list on to the 
#'  level one error generating function
#' @param arima_mod A list indicating the ARIMA model to pass to arima.sim. 
#'             See \code{\link{arima.sim}} for examples.
#' @param contrasts An optional list that specifies the contrasts to be used 
#'      for factor variables (i.e. those variables with .f or .c). 
#'      See \code{\link{contrasts}} for more detail.
#' @param ... Not currently used.
#' @export 
#' @examples 
#' #' # Three level example
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
#'    random_param3, cov_param, k,n, p, error_var, with_err_gen, 
#'    data_str = data_str)
#' 
sim_reg_nested3 <- function(fixed, random, random3, fixed_param, 
                            random_param = list(), random_param3 = list(), 
                            cov_param, k, n, p, error_var, with_err_gen, 
                            arima = FALSE, data_str, cor_vars = NULL, 
                            fact_vars = list(NULL), unbal = FALSE, 
                            unbal3 = FALSE, unbalCont = NULL, unbalCont3 = NULL,
                            lvl1_err_params = NULL, arima_mod = list(NULL),
                            contrasts = NULL, ...) {

  fixed_vars <- attr(terms(fixed),"term.labels")   
  rand.vars <- attr(terms(random),"term.labels")   
  rand.vars3 <- attr(terms(random3),"term.labels") 
  
  if(length(rand.vars)+1 != length(random_param$random_var)) 
    stop("Random lengths not equal")
  if(length(rand.vars3)+1 != length(random_param3$random_var)) 
    stop("Third level random lengths not equal")
  
  if(unbal3 == FALSE) {
    lvl2ss <- rep(n, k)
    n <- sum(lvl2ss)
  } else {
    if(length(unbalCont3) < 2) 
      stop("Must specify unbalCont3 when unbal3 = TRUE")
    lvl2ss <- round(runif(n = k, min = min(unbalCont3), 
                          max = max(unbalCont3)), 0)
    n <- sum(lvl2ss)
  }
  
  if(unbal == FALSE) {
    lvl1ss <- rep(p, n)
    if(is.null(lvl1ss)) stop("lvl1ss is NULL")
  } else {
    if(length(unbalCont) < 2) stop("Must specify unbalCont when unbal = TRUE")
    lvl1ss <- round(runif(n = n, min = min(unbalCont), max = max(unbalCont)), 0)
  }
  
  end <- cumsum(lvl2ss)
  beg <- c(1, cumsum(lvl2ss) + 1)
  beg <- beg[-length(beg)]
  
  lvl3ss <- sapply(lapply(1:length(beg), function(xx) 
    lvl1ss[beg[xx]:end[xx]]), sum)
  
  rand_eff <- do.call(sim_rand_eff, c(random_param, n = n))
  rand_eff3 <- do.call(sim_rand_eff, c(random_param3, n = k))
  
  Xmat <- sim_fixef_nested3(fixed, fixed_vars, cov_param, k, n = lvl2ss, 
                            p = lvl1ss, data_str = data_str, 
                            cor_vars = cor_vars, 
                            fact_vars = fact_vars, contrasts = contrasts)
  
  if(ncol(Xmat) != length(fixed_param)) {
    stop(paste(length(fixed_param), 'parameters specified for', ncol(Xmat), 
               'variables in design matrix'))
  }
  
  reff <- do.call("cbind", lapply(1:ncol(rand_eff), function(xx) 
    rep(rand_eff[,xx], times = lvl1ss)))
  colnames(reff) <- c(unlist(lapply(1:ncol(rand_eff), function(xx) 
    paste("b", xx-1, "_2", sep = ""))))
  
  reff3 <- do.call("cbind", lapply(1:ncol(rand_eff3), function(xx) 
    rep(rand_eff3[,xx], times = lvl3ss)))
  colnames(reff3) <- c(unlist(lapply(1:ncol(rand_eff3), function(xx) 
    paste("b", xx-1, "_3", sep = ""))))
  
  Zmat <- model.matrix(random, data.frame(Xmat))
  Zmat3 <- model.matrix(random3, data.frame(Xmat))
  
  err <- sim_err_nested(error_var, n = n, p = lvl1ss, 
                        with_err_gen = with_err_gen, arima = arima, 
                        lvl1_err_params = lvl1_err_params, 
                        arima_mod = arima_mod, ...)
  
  sim_data <- data_reg_nested3(Xmat, Zmat, Zmat3, fixed_param, rand_eff, 
                               rand_eff3, k, n = lvl2ss, p = lvl1ss, err = err)
  
  Xmat <- data.frame(Xmat, reff, reff3, sim_data)
  Xmat$withinID <- unlist(lapply(1:length(lvl1ss), function(xx) 1:lvl1ss[xx]))
  Xmat$clustID <- rep(1:n, times = lvl1ss)
  Xmat$clust3ID <- rep(1:k, times = lvl3ss)
 
  Xmat
}

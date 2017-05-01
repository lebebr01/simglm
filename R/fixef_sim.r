#' Simulates design matrix.
#' 
#' Input fixed variables, sample size, and number of within variables, 
#' returns design matrix.
#' 
#' Simulates the fixed effects for the \code{\link{sim_reg}} function when a 
#' linear mixed model is specified.  This function assumes a time variable 
#' when longitudinal data is specified and does include any interactions that 
#' are specified.
#' 
#' @param fixed One sided formula for fixed effects in the simulation.
#' @param fixed_vars Character vector of covariates for design matrix.
#' @param cov_param List of arguments to pass to the continuous generating 
#'   function. Required arguments include:
#'   \itemize{
#'     \item dist_fun: This is a quoted R distribution function.
#'     \item var_type: This is the level of variable to generate. Must be 
#'       either 'level1' or 'level2'. 
#'       Must be same order as fixed formula 
#'       above.
#'   }
#'   Optional arguments to the distribution functions are in a nested list,
#'    see the examples for example code for this.
#'  Does not include intercept, time, factors, or interactions.
#' @param n Number of clusters.
#' @param p Number of within cluster units.
#' @param data_str Type of data. Must be "cross", or "long".
#' @param cor_vars A vector of correlations between variables.
#' @param fact_vars A nested list of factor, categorical, or ordinal variable 
#'      specification, each list must include:
#'   \itemize{
#'        \item numlevels = Number of levels for ordinal or factor variables.
#'        \item var_type = Must be 'level1' or 'level2'.
#'    }
#'    Optional arguments include:
#'    \itemize{
#'        \item replace
#'        \item prob
#'        \item value.labels
#'    }
#'     See also \code{\link{sample}} for use of these optional arguments.
#' @param contrasts An optional list that specifies the contrasts to be used 
#'  for factor variables (i.e. those variables with .f or .c). 
#'  See \code{\link{contrasts}} for more detail.
#' @importFrom purrr pmap invoke_map
#' @export 
sim_fixef_nested <- function(fixed, fixed_vars, cov_param, n, p, data_str, 
                             cor_vars = NULL, fact_vars = list(NULL), 
                             contrasts = NULL){
  
  n.vars <- length(fixed_vars)
  n.int <- length(grep(":",fixed_vars))
  if(n.int > 0) {
    int.loc <- grep(":", fixed_vars)
  } else {
    int.loc <- 0
  }
  fact.loc <- grep("\\.f$|\\.o$|\\.c$|_f$|_c$|_o$", 
                   fixed_vars, ignore.case = TRUE) 
  w.var <- length(grep("level1", cov_param$var_type, ignore.case = TRUE))
  n.cont <- length(cov_param[[1]])
  
  if(length(fact.loc) > 0){
    fixed_vars <- c(fixed_vars[-c(fact.loc, int.loc)], fixed_vars[fact.loc], 
                    fixed_vars[int.loc])
  }
  
  if(length(fact.loc) > 0){
    n.fact <- ifelse(length(int.loc) > 0, length(fact.loc[fact.loc != int.loc]), 
                     length(fact.loc))
    n.fact.lvl1 <- length(grep("level1", fact_vars$var_type, ignore.case = TRUE))
    n.fact.lvl2 <- length(grep("level2", fact_vars$var_type, ignore.case = TRUE))
  } else {
    n.fact <- 0
  } 

  if(n.fact > 0){
    if(any(grepl("single", fact_vars$var_type))){
      stop("All variables must have var_type != 'single' for multilevel models")
    }
  }
  if(!is.null(cov_param)) {

    cov_param_args <- lapply(seq_len(n.cont), function(xx) 
      c(cov_param$dist_fun[[xx]], cov_param$var_type[[xx]], 
        cov_param$opts[[xx]]))
    
    Xmat <- do.call(cbind, purrr::invoke_map(lapply(seq_len(n.cont), 
                                                   function(xx) sim_continuous),
                                             cov_param_args, 
                                             n = n,
                                             k = NULL,
                                             p = p
    ))
    
    if(!is.null(cor_vars)) {
      Xmat <- corr_variables(Xmat, cor_vars, cov_param, standardize = TRUE)
    }
    if(data_str == "long") {
      if(is.null(cov_param$time_var)) {
        Xmat <- cbind(unlist(lapply(seq_along(p), function(xx) (1:p[xx]) - 1)), 
                      Xmat)
      } else {
        Xmat <- cbind(unlist(lapply(seq_along(p), function(xx) 
          cov_param$time_var[1:p[xx]])), 
                      Xmat)
      }
    }
  } else {
    if(data_str == 'long') {
      if(is.null(cov_param$time_var)) {
        Xmat <- unlist(lapply(seq_along(p), function(xx) (1:p[xx]) - 1))
      } else {
        Xmat <- unlist(lapply(seq_along(p), function(xx) 
          cov_param$time_var[1:p[xx]]))
      }
    } else {
      Xmat <- NULL
    }
  }

  if(length(fact.loc) > 0) {
    fact_vars <- c(list(k = lapply(seq_len(n.fact), function(xx) 0), 
                         n = lapply(seq_len(n.fact), function(xx) n), 
                         p = lapply(seq_len(n.fact), function(xx) p)), 
                    fact_vars)
    Xmat <- cbind(Xmat,  do.call(cbind, purrr::pmap(fact_vars, sim_factor)))
  }
  
   if(n.int == 0){
     colnames(Xmat) <- fixed_vars
   } else {
     int.loc <- grep(":", fixed_vars)
     colnames(Xmat) <- fixed_vars[-int.loc]
   } 
 if(any(grepl("\\.f$|\\.c$|_f$|_c$", fixed_vars, ignore.case = TRUE))) {
   fixed <- search_factors(fixed_vars)
 }
 Xmat <- model.matrix(fixed, data.frame(Xmat), contrasts.arg = contrasts)
 
 Xmat
}

#' Simulates design matrix.
#' 
#' Input fixed variables, sample size, and number of within variables, returns 
#'design matrix.
#' 
#' Simulates the fixed effects for the \code{\link{sim_reg}} function when a 
#' linear mixed model is specified.  This function assumes a time variable when 
#' longitudinal data is specified and does include any interactions that are 
#' specified.
#' 
#' @param fixed One sided formula for fixed effects in the simulation.
#' @param fixed_vars Character vector of covariates for design matrix.
#' @param cov_param List of arguments. Required arguments are:
#'   \itemize{
#'     \item dist_fun: This is a quoted R distribution function.
#'     \item var_type: This is the level of variable to generate. Must be 
#'       either 'level1', 'level2', or 'level3'. Must be same order as fixed formula 
#'       above.
#'   }
#'   Optional arguments to the distribution functions are in a nested list,
#'    see the examples for example code for this.
#'  Does not include intercept, time, factors, or interactions.
#' @param k Number of third level clusters.
#' @param n Number of clusters.
#' @param p Number of within cluster units.
#' @param data_str Type of data. Must be "cross", or "long".
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
#' @param contrasts An optional list that specifies the contrasts to be used for factor
#'      variables (i.e. those variables with .f or .c). See \code{\link{contrasts}} for 
#'      more detail.
#' @importFrom purrr pmap invoke_map
#' @export 
sim_fixef_nested3 <- function(fixed, fixed_vars, cov_param, k, n, p, data_str, 
                             cor_vars = NULL, fact_vars = list(NULL),
                             contrasts = NULL){
  
  n.vars <- length(fixed_vars)
  n.int <- length(grep(":",fixed_vars))
  if(n.int > 0) {
    int.loc <- grep(":", fixed_vars)
  } else {
    int.loc <- 0
  }
  fact.loc <- grep("\\.f$|\\.o$|\\.c$|_f$|_c$|_o$", 
                   fixed_vars, ignore.case = TRUE) 
  n.cont <- length(cov_param[[1]])
  
  if(length(fact.loc) > 0){
    fixed_vars <- c(fixed_vars[-c(fact.loc, int.loc)], fixed_vars[fact.loc], 
                    fixed_vars[int.loc])
  }
  
  if(length(fact.loc) > 0){
    n.fact <- ifelse(length(int.loc) > 0, length(fact.loc[fact.loc != int.loc]), 
                     length(fact.loc))
  } else {
    n.fact <- 0
  } 
  
  if(n.fact > 0){
    if(any(grepl("single", fact_vars$var_type))){
      stop("All variables must have var_type != 'single' for multilevel models")
    }
  }
  if(!is.null(cov_param)) {
    cov_param_args <- lapply(seq_len(n.cont), function(xx) 
      c(cov_param$dist_fun[[xx]], cov_param$var_type[[xx]], 
        cov_param$opts[[xx]]))
    
    Xmat <- do.call(cbind, purrr::invoke_map(lapply(seq_len(n.cont), 
                                                  function(xx) sim_continuous),
                                             cov_param_args, 
                                             n = n,
                                             k = k,
                                             p = p
    ))
    
    if(!is.null(cor_vars)) {
      Xmat <- corr_variables(Xmat, cor_vars, cov_param, standardize = TRUE)
    }
    if(data_str == "long") {
      if(is.null(cov_param$time_var)) {
        Xmat <- cbind(unlist(lapply(seq_along(p), function(xx) (1:p[xx]) - 1)), 
                      Xmat)
      } else {
        Xmat <- cbind(unlist(lapply(seq_along(p), function(xx) 
          cov_param$time_var[1:p[xx]])), 
                      Xmat)
      }
    }
  } else {
    if(data_str == 'long') {
      if(is.null(cov_param$time_var)) {
        Xmat <- unlist(lapply(seq_along(p), function(xx) (1:p[xx]) - 1))
      } else {
        Xmat <- unlist(lapply(seq_along(p), function(xx) 
          cov_param$time_var[1:p[xx]]))
      }
    } else {
      Xmat <- NULL
    }
  }
  
  if(length(fact.loc) > 0) {
    fact_vars <- c(list(k = lapply(seq_len(n.fact), function(xx) k), 
                        n = lapply(seq_len(n.fact), function(xx) n), 
                        p = lapply(seq_len(n.fact), function(xx) p)), 
                   fact_vars)
    Xmat <- cbind(Xmat,  do.call(cbind, purrr::pmap(fact_vars, sim_factor)))
  }
  
  if(n.int == 0){
    colnames(Xmat) <- fixed_vars
  } else {
    int.loc <- grep(":", fixed_vars)
    colnames(Xmat) <- fixed_vars[-int.loc]
  } 
  if(any(grepl("\\.f$|\\.c$|_f$|_c$", fixed_vars, ignore.case = TRUE))) {
    fixed <- search_factors(fixed_vars)
  }
  Xmat <- model.matrix(fixed, data.frame(Xmat), contrasts.arg = contrasts)
  
  Xmat
}


#' Simulates design matrix for single level model.
#' 
#' Input fixed variables, sample size, and number of within variables, 
#' returns design matrix.
#' 
#' Simulates the fixed effects for the \code{\link{sim_reg}} function when 
#' simulating a simple regression model.
#' 
#' @param fixed One sided formula for fixed effects in the simulation.
#' @param fixed_vars Character vector of covariates for design matrix.
#' @param n Number of clusters.
#' @param cov_param List of arguments to pass to the continuous generating 
#'   function. Required arguments include:
#'   \itemize{
#'     \item dist_fun: This is a quoted R distribution function.
#'     \item var_type: This is the level of variable to generate. Must be 
#'       'single'. Must be same order as fixed formula above.
#'   }
#'   Optional arguments to the distribution functions are in a nested list,
#'    see the examples for example code for this.
#'  Does not include intercept, time, factors, or interactions.
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
#' @param contrasts An optional list that specifies the contrasts to be used 
#'  for factor variables (i.e. those variables with .f or .c). 
#'  See \code{\link{contrasts}} for more detail.
#' @importFrom purrr pmap invoke_map
#' @export 
sim_fixef_single <- function(fixed, fixed_vars, n, cov_param, cor_vars = NULL, 
                             fact_vars = list(NULL), contrasts = NULL){
  
  n.vars <- length(fixed_vars)
  n.int <- length(grep(":",fixed_vars))
  if(n.int > 0) {
    int.loc <- grep(":", fixed_vars)
  } else {
    int.loc <- 0
  }
  fact.loc <- grep("\\.f$|\\.o$|\\.c$|_f$|_c$|_o$", 
                   fixed_vars, ignore.case = TRUE)  
  n.fact <- length(fact.loc[fact.loc != int.loc])
  n.cont <- length(cov_param[[1]])
  
  if(length(fact.loc) > 0){
    fixed_vars <- c(fixed_vars[-c(fact.loc, int.loc)], fixed_vars[fact.loc], 
                    fixed_vars[int.loc])
  }
  
  if(n.fact > 0){
    if(!any(grepl("single", fact_vars$var_type))){
      stop("All variables must have var_type = 'single'")
    }
  }
  if(!is.null(cov_param)) {
    cov_param_args <- lapply(seq_len(n.cont), function(xx) 
      c(cov_param$dist_fun[[xx]], cov_param$var_type[[xx]], 
        cov_param$opts[[xx]]))
    
    Xmat <- do.call(cbind, purrr::invoke_map(lapply(seq_len(n.cont), 
                                                  function(xx) sim_continuous),
                                             cov_param_args, 
                                             n = n,
                                             k = NULL,
                                             p = NULL
                                             ))
    
    if(!is.null(cor_vars)) {
      Xmat <- corr_variables(Xmat, cor_vars, cov_param, standardize = TRUE)
    }
  } else {
    Xmat <- NULL
  }

  if(length(fact.loc > 0)) {
    fact_vars <- c(list(k = rep(0, n.fact), n = rep(n, n.fact), 
                         p = rep(0, n.fact)), fact_vars)
    Xmat <- cbind(Xmat,  do.call(cbind, purrr::pmap(fact_vars, sim_factor)))
  }
  
  if(n.int == 0){
    colnames(Xmat) <- fixed_vars
  } else {
    int.loc <- grep(":", fixed_vars)
    colnames(Xmat) <- fixed_vars[-int.loc]
  } 
  if(any(grepl("\\.f$|\\.c$|_f$|_c$", fixed_vars, ignore.case = TRUE))) {
    fixed <- search_factors(fixed_vars)
  }
  Xmat <- model.matrix(fixed, data.frame(Xmat), contrasts.arg = contrasts)
  
  Xmat
}

#' Simulate categorical, factor, or discrete variables
#' 
#' Function that simulates discrete, factor, or categorical variables.  
#' Is essentially a wrapper around the sample function from base R.
#' 
#' @param k Number of third level clusters.
#' @param n Number of clusters or number of observations for single level
#' @param p Number of within cluster observations for multilevel
#' @param numlevels Scalar indicating the number of levels for categorical, 
#'   factor, or discrete variable
#' @param replace Whether to replace levels of categorical variable, TRUE/FALSE
#' @param prob Probability of levels for variable, must be same length as 
#'  numlevels
#' @param var_type Variable type for the variable, must be either 
#'   "level1", "level2", "level3", or "single"
#' @param value_labels Optional argument with value labels for variable, 
#'        converts variable to factor.
#' @export 
sim_factor <- function(k = NULL, n, p, numlevels, replace = TRUE, prob = NULL, 
                       var_type = c('level1', 'level2', 'level3', 'single'), 
                       value_labels = NULL) {

  if(var_type == 'single' | var_type == 'level2') {
    if(replace == FALSE & numlevels < n) {
      stop("If replace = FALSE, numlevels must be greater than n for level2 or single")
    }
  }
  if(var_type == "level1") {
    if(replace == FALSE & numlevels < sum(p)){
      stop("If replace = FALSE, numlevels must be greater than sum(p) for level1")
    }
  }
  if(var_type == "level3") {
    if(replace == FALSE & numlevels < k) {
      stop("If replace = FALSE, numlevels must be greater than k for level3")
    }
  }
  
  end <- cumsum(n)
  beg <- c(1, cumsum(n) + 1)
  beg <- beg[-length(beg)]
  
  if(!is.null(k)) {
    lvl3ss <- sapply(lapply(seq_along(beg), function(xx) 		
      p[beg[xx]:end[xx]]), sum)
  }
  
  var_type <- match.arg(var_type)
  
  cat_var <- switch(var_type,
         single = sample(x = numlevels, size = n, replace = replace, 
                         prob = prob),
         level3 = rep(sample(x = numlevels, size = k, replace = replace, 
                           prob = prob), times = lvl3ss),
         level2 = rep(sample(x = numlevels, size = length(p), replace = replace, 
                           prob = prob), times = p),
         level1 = sample(x = numlevels, size = sum(p), replace = replace, 
                       prob = prob)
         )
  
  if(!is.null(value_labels)) {
    if(length(value_labels) != numlevels) { 
      stop("value_labels must be same length as numlevels") 
      }
    cat_var <- factor(cat_var, labels = value_labels)
  }
  
  cat_var
}

#' Simulate continuous variables
#' 
#' Function that simulates continuous variables. Any distribution function in 
#' R is supported.
#' 
#' @param k Number of third level clusters.
#' @param n Number of clusters or number of observations for single level
#' @param p Number of within cluster observations for multilevel
#' @param dist_fun A distribution function. This argument takes a quoted
#'      R distribution function (e.g. 'rnorm').
#' @param var_type Variable type for the variable, must be either "level1", 
#'      "level2", "level3", or "single"
#' @param ... Additional parameters to pass to the dist_fun argument.
#' @export 
sim_continuous <- function(k = NULL, n, p, dist_fun,
                           var_type = c('level1', 'level2', 'level3', 'single'),
                           ...) {
  
  end <- cumsum(n)
  beg <- c(1, cumsum(n) + 1)
  beg <- beg[-length(beg)]
  
  if(!is.null(k)) {
    lvl3ss <- sapply(lapply(seq_along(beg), function(xx) 		
      p[beg[xx]:end[xx]]), sum)
  }
  
  var_type <- match.arg(var_type)
  
  contVar <- switch(var_type,
                   single = unlist(lapply(n, FUN = dist_fun, ...)),
                   level3 = rep(unlist(lapply(k, FUN = dist_fun, ...)), 
                              times = lvl3ss),
                   level2 = rep(unlist(lapply(length(p), FUN = dist_fun, ...)), 
                              times = p),
                   level1 = unlist(lapply(sum(p), FUN = dist_fun, ...))
  )
  contVar
}

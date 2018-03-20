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
#'        \item numlevels: Number of levels for ordinal or factor variables.
#'        \item var_type: Must be 'level1' or 'level2'.
#'    }
#'    Optional arguments passed on to sample in a nested list. These include:
#'    \itemize{
#'        \item replace
#'        \item prob
#'        \item value.labels
#'    }
#'     See also \code{\link{sample}} for use of these optional arguments.
#' @param contrasts An optional list that specifies the contrasts to be used 
#'  for factor variables (i.e. those variables with .f or .c). 
#'  See \code{\link{contrasts}} for more detail.
#' @param knot_args A nested list of named knot arguments. See \code{\link{sim_knot}} 
#'  for more details. Arguments must include:
#'    \itemize{
#'      \item var
#'      \item knot_locations
#'    }
#' @importFrom purrr pmap invoke_map
#' @export 
sim_fixef_nested <- function(fixed, fixed_vars, cov_param, n, p, data_str, 
                             cor_vars = NULL, fact_vars = list(NULL), 
                             contrasts = NULL, knot_args = list(NULL)) {
  
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
  
  knot_loc <- grep("\\.k$|_k$", fixed_vars, ignore.case = TRUE)
  n_knot <- length(knot_loc[knot_loc %ni% int.loc])
  knot_var_loc <- grep(paste0(knot_args$var, '$'), fixed_vars)
  
  if(length(fact.loc) > 0){
    if(length(knot_loc) > 0) {
      fixed_vars <- c(fixed_vars[-c(fact.loc, int.loc, knot_loc)], 
                      fixed_vars[fact.loc[fact.loc %ni% int.loc]], fixed_vars[knot_loc],
                      fixed_vars[int.loc])
    } else {
      fixed_vars <- c(fixed_vars[-c(fact.loc, int.loc)], 
                      fixed_vars[fact.loc[fact.loc %ni% int.loc]],
                      fixed_vars[int.loc])
    }
  }
  
  if(length(fact.loc) > 0){
    n.fact <- ifelse(length(int.loc) > 0, length(fact.loc[fact.loc %ni% int.loc]), 
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
    fact_vars_args <- lapply(seq_len(n.fact), function(xx)
      c(fact_vars$numlevels[[xx]], 
        fact_vars$var_type[[xx]],
        fact_vars$opts[[xx]])
    )
    
    Xmat <- cbind(Xmat, do.call(cbind, purrr::invoke_map(lapply(seq_len(n.fact), 
                                                                function(xx) sim_factor),
                                                         fact_vars_args, 
                                                         n = n,
                                                         k = NULL,
                                                         p = p
    ))
    )
  }
  
  if(length(knot_loc) > 0) {
    Xmat <- cbind(Xmat, do.call(cbind, 
                                purrr::invoke_map(lapply(seq_len(n_knot), 
                                                         function(xx) sim_knot),
                                                  knot_args$knot_locations,
                                                  var = Xmat[, knot_var_loc])))
  }
  
  if(n.int == 0){
    colnames(Xmat) <- fixed_vars
  } else {
    int.loc <- grep(":", fixed_vars)
    colnames(Xmat) <- fixed_vars[-int.loc]
  } 
  if(any(grepl("\\.f$|\\.c$|_f$|_c$|\\.k$|_k$", fixed_vars, ignore.case = TRUE))) {
    fixed <- search_factors(fixed_vars)
    Omat <- Xmat
  }
  
  Xmat <- model.matrix(fixed, data.frame(Xmat), contrasts.arg = contrasts)
  
  if(any(grepl("\\.f$|\\.c$|_f$|_c$|\\.k$|_k$", fixed_vars, ignore.case = TRUE))) {
    list(Xmat = Xmat, Omat = data.frame(Omat))
  } else {
    Xmat
  }
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
#'    Optional arguments passed on to sample in a nested list. These include:
#'    \itemize{
#'        \item replace
#'        \item prob
#'        \item value.labels
#'    }
#'     See also \code{\link{sample}} for use of these optional arguments.
#' @param contrasts An optional list that specifies the contrasts to be used for factor
#'      variables (i.e. those variables with .f or .c). See \code{\link{contrasts}} for 
#'      more detail.
#' @param knot_args A nested list of named knot arguments. See \code{\link{sim_knot}} 
#'  for more details. Arguments must include:
#'    \itemize{
#'      \item var
#'      \item knot_locations
#'    }
#' @importFrom purrr pmap invoke_map
#' @export 
sim_fixef_nested3 <- function(fixed, fixed_vars, cov_param, k, n, p, data_str, 
                              cor_vars = NULL, fact_vars = list(NULL),
                              contrasts = NULL, knot_args = list(NULL)) {
  
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
  
  knot_loc <- grep("\\.k$|_k$", fixed_vars, ignore.case = TRUE)
  n_knot <- length(knot_loc[knot_loc %ni% int.loc])
  knot_var_loc <- grep(paste0(knot_args$var, '$'), fixed_vars)
  
  if(length(fact.loc) > 0){
    if(length(knot_loc) > 0) {
      fixed_vars <- c(fixed_vars[-c(fact.loc, int.loc, knot_loc)], 
                      fixed_vars[fact.loc[fact.loc %ni% int.loc]], fixed_vars[knot_loc],
                      fixed_vars[int.loc])
    } else {
      fixed_vars <- c(fixed_vars[-c(fact.loc, int.loc)], 
                      fixed_vars[fact.loc[fact.loc %ni% int.loc]],
                      fixed_vars[int.loc])
    }
  }
  
  if(length(fact.loc) > 0){
    n.fact <- ifelse(length(int.loc) > 0, length(fact.loc[fact.loc %ni% int.loc]), 
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
    fact_vars_args <- lapply(seq_len(n.fact), function(xx)
      c(fact_vars$numlevels[[xx]], 
        fact_vars$var_type[[xx]],
        fact_vars$opts[[xx]])
    )
    
    Xmat <- cbind(Xmat, do.call(cbind, purrr::invoke_map(lapply(seq_len(n.fact), 
                                                                function(xx) sim_factor),
                                                         fact_vars_args, 
                                                         n = n,
                                                         k = k,
                                                         p = p
    ))
    )
  }
  
  if(length(knot_loc) > 0) {
    Xmat <- cbind(Xmat, do.call(cbind, 
                                purrr::invoke_map(lapply(seq_len(n_knot), 
                                                         function(xx) sim_knot),
                                                  knot_args$knot_locations,
                                                  var = Xmat[, knot_var_loc])))
  }
  
  if(n.int == 0){
    colnames(Xmat) <- fixed_vars
  } else {
    int.loc <- grep(":", fixed_vars)
    colnames(Xmat) <- fixed_vars[-int.loc]
  } 
  if(any(grepl("\\.f$|\\.c$|_f$|_c$|\\.k$|_k$", fixed_vars, ignore.case = TRUE))) {
    fixed <- search_factors(fixed_vars)
    Omat <- Xmat
  }
  Xmat <- model.matrix(fixed, data.frame(Xmat), contrasts.arg = contrasts)
  
  if(any(grepl("\\.f$|\\.c$|_f$|_c$|\\.k$|_k$", fixed_vars, ignore.case = TRUE))) {
    list(Xmat = Xmat, Omat = data.frame(Omat))
  } else {
    Xmat
  }
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
#'    Optional arguments passed on to sample in a nested list. These include:
#'    \itemize{
#'        \item replace
#'        \item prob
#'        \item value.labels
#'    }
#'     See also \code{\link{sample}} for use of these optional arguments.
#' @param contrasts An optional list that specifies the contrasts to be used 
#'  for factor variables (i.e. those variables with .f or .c). 
#'  See \code{\link{contrasts}} for more detail.
#' @param knot_args A nested list of named knot arguments. See \code{\link{sim_knot}} 
#'  for more details. Arguments must include:
#'    \itemize{
#'      \item var
#'      \item knot_locations
#'    }
#' @importFrom purrr pmap invoke_map
#' @export 
sim_fixef_single <- function(fixed, fixed_vars, n, cov_param, cor_vars = NULL, 
                             fact_vars = list(NULL), contrasts = NULL,
                             knot_args = list(NULL)) {
  
  n.vars <- length(fixed_vars)
  n.int <- length(grep(":",fixed_vars))
  if(n.int > 0) {
    int.loc <- grep(":", fixed_vars)
  } else {
    int.loc <- 0
  }
  fact.loc <- grep("\\.f$|\\.o$|\\.c$|_f$|_c$|_o$", 
                   fixed_vars, ignore.case = TRUE)  
  n.fact <- length(fact.loc[fact.loc %ni% int.loc])
  n.cont <- length(cov_param[[1]])
  
  knot_loc <- grep("\\.k$|_k$", fixed_vars, ignore.case = TRUE)
  n_knot <- length(knot_loc[knot_loc %ni% int.loc])
  knot_var_loc <- grep(paste0(knot_args$var, '$'), fixed_vars)
  
  if(length(fact.loc) > 0){
    if(length(knot_loc) > 0) {
      fixed_vars <- c(fixed_vars[-c(fact.loc, int.loc, knot_loc)], 
                      fixed_vars[fact.loc[fact.loc %ni% int.loc]], fixed_vars[knot_loc],
                      fixed_vars[int.loc])
    } else {
      fixed_vars <- c(fixed_vars[-c(fact.loc, int.loc)], 
                      fixed_vars[fact.loc[fact.loc %ni% int.loc]],
                      fixed_vars[int.loc])
    }
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
  
  if(length(fact.loc) > 0) {
    fact_vars_args <- lapply(seq_len(n.fact), function(xx)
      c(fact_vars$numlevels[[xx]], 
        fact_vars$var_type[[xx]],
        fact_vars$opts[[xx]])
    )
    
    Xmat <- cbind(Xmat, do.call(cbind, purrr::invoke_map(lapply(seq_len(n.fact), 
                                                                function(xx) sim_factor),
                                                         fact_vars_args, 
                                                         n = n,
                                                         k = NULL,
                                                         p = NULL
    ))
    )
  }
  
  if(length(knot_loc) > 0) {
    Xmat <- cbind(Xmat, do.call(cbind, 
                                purrr::invoke_map(lapply(seq_len(n_knot), 
                                                         function(xx) sim_knot),
                                                  knot_args$knot_locations,
                                                  var = Xmat[, knot_var_loc])))
  }
  
  if(n.int == 0){
    colnames(Xmat) <- fixed_vars
  } else {
    int.loc <- grep(":", fixed_vars)
    colnames(Xmat) <- fixed_vars[-int.loc]
  } 
  if(any(grepl("\\.f$|\\.c$|_f$|_c$|\\.k$|_k$", fixed_vars, ignore.case = TRUE))) {
    fixed <- search_factors(fixed_vars)
    Omat <- Xmat
  }
  Xmat <- model.matrix(fixed, data.frame(Xmat), contrasts.arg = contrasts)
  
  if(any(grepl("\\.f$|\\.c$|_f$|_c$|\\.k$|_k$", fixed_vars, ignore.case = TRUE))) {
    list(Xmat = Xmat, Omat = data.frame(Omat))
  } else {
    Xmat
  }
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
#' @param var_type Variable type for the variable, must be either 
#'   "level1", "level2", "level3", or "single"
#' @param ... Additional parameters passed to the sample function.
#' @export 
sim_factor <- function(k = NULL, n, p, numlevels, 
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
  
  cat_var <- switch(var_type,
                    single = base::sample(x = numlevels, size = n, ...),
                    level3 = rep(base::sample(x = numlevels, size = k, ...), times = lvl3ss),
                    level2 = rep(base::sample(x = numlevels, size = length(p), ...), times = p),
                    level1 = base::sample(x = numlevels, size = sum(p), ...)
  )
  cat_var
}


#' Simulate categorical, factor, or discrete variables
#' 
#' Function that simulates discrete, factor, or categorical variables.  
#' Is essentially a wrapper around the sample function from base R.
#' 
#' @param n A list of sample sizes.
#' @param levels Scalar indicating the number of levels for categorical, 
#'   factor, or discrete variable. Can also specify levels as a character vector.
#' @param var_level The level the variable should be simulated at. This can either 
#'      be 1, 2, or 3 specifying a level 1, level 2, or level 3 variable 
#'      respectively.
#' @param replace TRUE/FALSE indicating whether levels should be sampled with 
#'   replacement. Default is TRUE.
#' @param ... Additional parameters passed to the sample function.
#' @export 
sim_factor2 <- function(n, levels, var_level = 1, replace = TRUE,
                       ...) {
  if(var_level == 1) {
    cat_var <- base::sample(x = levels, size = n[['level1']], 
                            replace = replace, ...)
  } else {
    if(var_level == 2) {
      cat_var <- rep(base::sample(x = levels, size = sum(n[['level2']]), 
                                  replace = replace, ...),
                      times = n[['level1']])
    } else {
      cat_var <- rep(base::sample(x = levels, size = n[['level3']], 
                                  replace = replace, ...),
                      times = n[['level3_total']])
    }
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

#' Simulate continuous variables
#' 
#' Function that simulates continuous variables. Any distribution function in 
#' R is supported.
#' 
#' @param n A list of sample sizes.
#' @param dist A distribution function. This argument takes a quoted
#'      R distribution function (e.g. 'rnorm'). Default is 'rnorm'.
#' @param var_level The level the variable should be simulated at. This can either 
#'      be 1, 2, or 3 specifying a level 1, level 2, or level 3 variable 
#'      respectively.
#' @param variance The variance for random effect simulation.
#' @param ther_sim A TRUE/FALSE flag indicating whether the error simulation 
#'  function should be simulated, that is should the mean and standard deviation
#'  used for standardization be simulated. Can optionally be a vector of length
#'  two that directly specifies the theoretical mean/variance of the generated 
#'  distribution.
#' @param ... Additional parameters to pass to the dist_fun argument.
#' @export 
sim_continuous2 <- function(n, dist = 'rnorm', var_level = 1, 
                            variance = NULL, ther_sim = FALSE, ...) {
  
  if(var_level == 1) {
    cont_var <- unlist(lapply(n[['level1']], FUN = dist, ...))
  } else {
    if(var_level == 2) {
      cont_var <- rep(unlist(lapply(sum(n[['level2']]), FUN = dist, ...)),
                      times = n[['level1']])
    } else {
      cont_var <- rep(unlist(lapply(n[['level3']], FUN = dist, ...)),
                      times = n[['level3_total']])
    }
  }
  
  if(!is.null(variance)) {
    if(ther_sim) {
      ther_val <- do.call(dist, c(list(n = 10000000), ...))
      ther <- c(mean(ther_val), sd(ther_val))
      
      cont_var <- standardize(cont_var, ther[1], ther[2])
    }
    if(length(ther_sim) == 2) {
      cont_var <- standardize(cont_var, ther_sim[1], ther_sim[2])
    }
    
    cont_var <- cont_var %*% chol(c(sqrt(variance)))
  }
  
  cont_var
}

#' Simulate knot locations
#' 
#' Function that generates knot locations. An example of usefulness of this funciton
#' would be with generation of interrupted time series data. Another application may
#' be with simulation of piecewise linear data structures.
#' 
#' @param var Variable used to create knots in the data. 
#' @param knot_locations The locations to create knots. These need to be specified 
#'   with the scale of the variable in mind. See examples.
#' @param right logical, indicating if the intervals should be closed on the right
#'   (and open on the left) or vice versa. See \code{\link{cut}} for more details. 
#'   Defaults to FALSE, which is likely most desirable behavior in this context.
#' @export 
#' @examples
#' sim_knot(0:10, knot_locations = c(4, 9))
#' sim_knot(rnorm(100), knot_locations = c(-1, 1.5))
#' sim_knot(0:8, knot_locations = 5)   
#' sim_knot(0:8, knot_locations = 5, right = TRUE)  
sim_knot <- function(var, knot_locations, right = FALSE) {
  
  if(!right) {
    knot_locat <- c(min(var), knot_locations, (max(var) + 1))
  } else {
    knot_locat <- c((min(var) - 1), knot_locations, (max(var)))
  }
  
  cut(var, knot_locat, labels = FALSE, right = right) - 1
}

#' Simulate Time
#' 
#' This function simulates data for the time variable of longitudinal data.
#' 
#' @param n Sample size of the levels.
#' @param time_levels The values the time variable should take. If NULL (default),
#'   the time values are discrete integers starting at 0 and going to n - 1.
#' 
#' @export 
sim_time <- function(n, time_levels = NULL, ...) {
  
  if(is.null(time_levels)) {
    do.call('c', 
            lapply(seq_along(n[['level1']]), function(xx) 
              0:(n[['level1']][xx] - 1)))
  } else {
    do.call('c', 
            lapply(seq_along(n[['level1']]), function(xx)
              time_levels[1:n[['level1']][xx]]))
  }
  
}

sim_variable <- function(var_type = c("continuous", "factor", "ordinal", 
                                      "knot", 'time'), ...) {
  var_type <- match.arg(var_type)
  
  switch(var_type,
    continuous = sim_continuous2(...),
    factor = sim_factor2(...),
    ordinal = sim_factor2(...),
    knot = sim_knot(...),
    time = sim_time(...)
  )
}

#' Tidy fixed effect formula simulation
#' 
#' This function simulates the fixed portion of the model using a formula syntax.
#' 
#' @param data Data simulated from other functions to pass to this function. Can pass
#'  NULL if first in simulation string.
#' @param sim_args A named list with special model formula syntax. See details and examples
#'   for more information. The named list may contain the following:
#'   \itemize{
#'     \item fixed: This is the fixed portion of the model (i.e. covariates)
#'     \item random: This is the random portion of the model (i.e. random effects)
#'     \item error: This is the error (i.e. residual term).
#'   }
#' @param ... Other arguments to pass to error simulation functions.
#' @importFrom purrr modify_if
#' @examples 
#' 
#' @export 
simulate_fixed <- function(data, sim_args, ...) {
  
  fixed_formula <- parse_formula(sim_args)[['fixed']]
  
  fixed_vars <- attr(terms(fixed_formula), "term.labels")  
  
  if(any(grepl('^factor\\(', fixed_vars))) {
    fixed_vars <- gsub("factor\\(|\\)$", "", fixed_vars)
  }
  
  if(is.null(data)) {
    n <- sample_sizes(sim_args[['sample_size']])
    ids <- create_ids(n, 
                      c('level1_id', parse_randomeffect(parse_formula(sim_args)[['randomeffect']])[['cluster_id_vars']]))
    Xmat <- purrr::invoke_map("sim_variable", 
                              sim_args[['fixed']],
                              n = n
    ) %>% 
      data.frame()
  } else {
    n <- compute_samplesize(data, sim_args)
    Xmat <- purrr::invoke_map("sim_variable", 
                              sim_args[['fixed']],
                              n = n
    ) %>% 
      data.frame()
  }
  
  if(any(grepl(":", fixed_vars))) {
    int.loc <- grep(":", fixed_vars)
    colnames(Xmat) <- fixed_vars[-int.loc]
  } else {
    colnames(Xmat) <- fixed_vars
  } 
  if(any(unlist(lapply(seq_along(sim_args[['fixed']]), function(xx) 
    sim_args[['fixed']][[xx]]$var_type)) == 'factor')) {
    
    num_levels <- lapply(seq_along(sim_args[['fixed']]), function(xx) 
      sim_args[['fixed']][[xx]][['levels']])
    num_levels <- purrr::modify_if(num_levels, is.character, length)
    
    if(any(unlist(lapply(seq_along(sim_args[['fixed']]), function(xx) 
      num_levels[[xx]] > 2 & 
      sim_args[['fixed']][[xx]][['var_type']] == 'factor'))
      )) {
      fixed_vars <- factor_names(sim_args, fixed_vars)
    }
    
    Omat <- Xmat
    Xmat <- data.frame(model.matrix(fixed_formula, Xmat, ...))
    colnames(Xmat)[2:ncol(Xmat)] <- fixed_vars
    Xmat <- dplyr::bind_cols(Xmat, Omat)
  } else {
    Xmat <- data.frame(model.matrix(fixed_formula, Xmat, ...))
    colnames(Xmat)[2:ncol(Xmat)] <- fixed_vars
  }
  
  if(is.null(data)) {
    data.frame(Xmat, ids)
  } else {
    data.frame(data, Xmat)
  }
}

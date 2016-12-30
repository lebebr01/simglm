#' Simulates design matrix.
#' 
#' Input fixed variables, sample size, and number of within variables, returns design matrix.
#' 
#' Simulates the fixed effects for the \code{\link{sim_reg}} function when a linear mixed
#' model is specified.  This function assumes a time variable when longitudinal data 
#' is specified and does include any interactions that are specified.
#' 
#' @param fixed One sided formula for fixed effects in the simulation.
#' @param fixed_vars Character vector of covariates for design matrix.
#' @param cov_param List of mean, sd (standard deviations), and var_type for fixed effects. 
#'  Does not include intercept, time, factors, or interactions. 
#'  var_type must be either "lvl1" or "lvl2". Must be same order as fixed formula above.
#' @param n Number of clusters.
#' @param p Number of within cluster units.
#' @param data_str Type of data. Must be "cross", or "long".
#' @param cor_vars A vector of correlations between variables.
#' @param fact_vars A nested list of factor, categorical, or ordinal variable specification, 
#'      each list must include numlevels and var_type (must be "lvl1" or "lvl2");
#'      optional specifications are: replace, prob, value.labels.
#' @param contrasts An optional list that specifies the contrasts to be used for factor
#'      variables (i.e. those variables with .f or .c). See \code{\link{contrasts}} for 
#'      more detail.
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
  fact.loc <- grep("\\.f|\\.o|\\.c", fixed_vars, ignore.case = TRUE) 
  w.var <- length(grep("lvl1", cov_param$var_type, ignore.case = TRUE))
  n.cont <- length(cov_param$mean)
  
  cov_mu <- cov_param$mean
  cov_sd <- cov_param$sd
  
  if(length(fact.loc) > 0){
    fixed_vars <- c(fixed_vars[-c(fact.loc, int.loc)], fixed_vars[fact.loc], fixed_vars[int.loc])
  }
  
  if(length(fact.loc) > 0){
    n.fact <- ifelse(length(int.loc) > 0, length(fact.loc[fact.loc != int.loc]), 
                     length(fact.loc))
    n.fact.lvl1 <- length(grep("lvl1", fact_vars$var_type, ignore.case = TRUE))
    n.fact.lvl2 <- length(grep("lvl2", fact_vars$var_type, ignore.case = TRUE))
  } else {
    n.fact <- 0
  } 

  if(n.fact > 0){
    if(any(grepl("single", fact_vars$var_type))){
      stop("All variables must have var_type != 'single'")
    }
  }
  if(!is.null(cov_param)) {
    if(data_str == "long") {
      Xmat <- unlist(lapply(1:length(p), function(xx) (1:p[xx])-1))
      
      if(is.null(cor_vars)) {
        cov_param2 <- lapply(1:n.cont, function(xx) 
          list(k = 0, n = n, p = p, mean = cov_param$mean[xx], sd = cov_param$sd[xx], 
               var_type = cov_param$var_type[xx]))
      } else {
        cov_param2 <- lapply(1:n.cont, function(xx) 
          list(k = 0, n = n, p = p, mean = 0, sd = 1, 
               var_type = cov_param$var_type[xx]))
      }
      
      Xmat <- cbind(Xmat, do.call("cbind", lapply(1:n.cont, function(xx) 
        do.call(sim_continuous, cov_param2[[xx]]))))
    } else {
      if(is.null(cor_vars)) {
        cov_param2 <- lapply(1:n.cont, function(xx) 
          list(k = 0, n = n, p = p, mean = cov_param$mean[xx], sd = cov_param$sd[xx], 
               var_type = cov_param$var_type[xx]))
      } else {
        cov_param2 <- lapply(1:n.cont, function(xx) 
          list(k = 0, n = n, p = p, mean = 0, sd = 1, 
               var_type = cov_param$var_type[xx]))
      }
      Xmat <- do.call("cbind", lapply(1:n.cont, function(xx) 
        do.call(sim_continuous, cov_param2[[xx]])))
    } 
    
    if(!is.null(cor_vars)) {
      c_mat <- matrix(nrow = n.cont, ncol = n.cont)
      diag(c_mat) <- 1
      c_mat[upper.tri(c_mat)] <- c_mat[lower.tri(c_mat)] <- cor_vars
      cov <- diag(cov_sd) %*% c_mat %*% diag(cov_sd)
      es <- eigen(cov, symmetric = TRUE)
      ev <- es$values
      Xmat <- t(cov_mu + es$vectors %*% diag(sqrt(pmax(ev, 0)), length(cov_sd)) %*% 
                  t(Xmat))
    }
  } else {
    if(data_str == 'long') {
      Xmat <- unlist(lapply(1:length(p), function(xx) (1:p[xx])-1))
    } else {
      Xmat <- NULL
    }
  }

  if(length(fact.loc > 0)){
    fact_vars <- lapply(1:n.fact, function(xx) 
      list(k = NULL, n = n, p = p, numlevels = fact_vars$numlevels[xx], 
           var_type = fact_vars$var_type[xx]))
    Xmat <- cbind(Xmat, do.call("cbind", lapply(1:n.fact, 
              function(xx) do.call(sim_factor, fact_vars[[xx]]))))
  }
  
   if(n.int == 0){
     colnames(Xmat) <- fixed_vars
   } else {
     int.loc <- grep(":", fixed_vars)
     colnames(Xmat) <- fixed_vars[-int.loc]
   } 
 if(any(grepl("\\.f|\\.c", fixed_vars, ignore.case = TRUE))) {
   fixed <- search_factors(fixed_vars)
 }
 Xmat <- model.matrix(fixed, data.frame(Xmat), contrasts.arg = contrasts)
 Xmat
}

#' Simulates design matrix.
#' 
#' Input fixed variables, sample size, and number of within variables, returns design matrix.
#' 
#' Simulates the fixed effects for the \code{\link{sim_reg}} function when a linear mixed
#' model is specified.  This function assumes a time variable when longitudinal data 
#' is specified and does include any interactions that are specified.
#' 
#' @param fixed One sided formula for fixed effects in the simulation.
#' @param fixed_vars Character vector of covariates for design matrix.
#' @param cov_param List of mean, sd (standard deviations), and var_type for fixed effects. 
#'  Does not include intercept, time, factors, or interactions. 
#'  var_type must be either "lvl1", "lvl2", or "lvl3"". 
#'  Must be same order as fixed formula above.
#' @param k Number of third level clusters.
#' @param n Number of clusters.
#' @param p Number of within cluster units.
#' @param data_str Type of data. Must be "cross", or "long".
#' @param cor_vars A vector of correlations between variables.
#' @param fact_vars A nested list of factor, categorical, or ordinal variable specification, 
#'      each list must include numlevels and var_type (must be "lvl1", "lvl2", or "lvl3");
#'      optional specifications are: replace, prob, value.labels.
#' @param contrasts An optional list that specifies the contrasts to be used for factor
#'      variables (i.e. those variables with .f or .c). See \code{\link{contrasts}} for 
#'      more detail.
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
  fact.loc <- grep("\\.f|\\.o|\\.c", fixed_vars, ignore.case = TRUE) 
  n.cont <- length(cov_param$mean)
  
  cov_mu <- cov_param$mean
  cov_sd <- cov_param$sd
  
  if(length(fact.loc) > 0){
    fixed_vars <- c(fixed_vars[-c(fact.loc, int.loc)], fixed_vars[fact.loc], fixed_vars[int.loc])
  }
  
  if(length(fact.loc) > 0){
    n.fact <- ifelse(length(int.loc) > 0, length(fact.loc[fact.loc != int.loc]), 
                     length(fact.loc))
  } else {
    n.fact <- 0
  } 
  
  if(n.fact > 0){
    if(any(grepl("single", fact_vars$var_type))){
      stop("All variables must have var_type != 'single'")
    }
  }
  if(!is.null(cov_param)) {
    if(data_str == "long") {
      Xmat <- unlist(lapply(1:length(p), function(xx) (1:p[xx]) - 1))
      
      if(is.null(cor_vars)) {
        cov_param2 <- lapply(1:n.cont, function(xx) 
          list(k = k, n = n, p = p, mean = cov_param$mean[xx], sd = cov_param$sd[xx], 
               var_type = cov_param$var_type[xx]))
      } else {
        cov_param2 <- lapply(1:n.cont, function(xx) 
          list(k = k, n = n, p = p, mean = 0, sd = 1, 
               var_type = cov_param$var_type[xx]))
      }
      Xmat <- cbind(Xmat, do.call("cbind", lapply(1:n.cont, function(xx) 
        do.call(sim_continuous, cov_param2[[xx]]))))
    } else {
      if(is.null(cor_vars)) {
        cov_param2 <- lapply(1:n.cont, function(xx) 
          list(k = k, n = n, p = p, mean = cov_param$mean[xx], sd = cov_param$sd[xx], 
               var_type = cov_param$var_type[xx]))
      } else {
        cov_param2 <- lapply(1:n.cont, function(xx) 
          list(k = k, n = n, p = p, mean = 0, sd = 1, 
               var_type = cov_param$var_type[xx]))
      }
      Xmat <- do.call("cbind", lapply(1:n.cont, function(xx) 
        do.call(sim_continuous, cov_param2[[xx]])))
    }
    
    if(!is.null(cor_vars)) {
      c_mat <- matrix(nrow = n.cont, ncol = n.cont)
      diag(c_mat) <- 1
      c_mat[upper.tri(c_mat)] <- c_mat[lower.tri(c_mat)] <- cor_vars
      cov <- diag(cov_sd) %*% c_mat %*% diag(cov_sd)
      es <- eigen(cov, symmetric = TRUE)
      ev <- es$values
      Xmat <- t(cov_mu + es$vectors %*% diag(sqrt(pmax(ev, 0)), length(cov_sd)) %*% 
                  t(Xmat))
    }
  } else {
    if(data_str == 'long') {
      Xmat <- unlist(lapply(1:length(p), function(xx) (1:p[xx])-1))
    } else {
      Xmat <- NULL
    }
  }
  
  if(length(fact.loc > 0)){
    fact_vars <- lapply(1:n.fact, function(xx) 
      list(k = k, n = n, p = p, numlevels = fact_vars$numlevels[xx], 
           var_type = fact_vars$var_type[xx]))
    Xmat <- cbind(Xmat, do.call("cbind", lapply(1:n.fact, 
                function(xx) do.call(sim_factor, fact_vars[[xx]]))))
  }
  
  if(n.int == 0){
    colnames(Xmat) <- fixed_vars
  } else {
    int.loc <- grep(":", fixed_vars)
    colnames(Xmat) <- fixed_vars[-int.loc]
  } 
  if(any(grepl("\\.f|\\.c", fixed_vars, ignore.case = TRUE))) {
    fixed <- search_factors(fixed_vars)
  }
  Xmat <- model.matrix(fixed, data.frame(Xmat), contrasts.arg = contrasts)
  Xmat
}


#' Simulates design matrix for single level model.
#' 
#' Input fixed variables, sample size, and number of within variables, returns design matrix.
#' 
#' Simulates the fixed effects for the \code{\link{sim_reg}} function when simulating a 
#' simple regression model.
#' 
#' @param fixed One sided formula for fixed effects in the simulation.
#' @param fixed_vars Character vector of covariates for design matrix.
#' @param n Number of clusters.
#' @param cov_param List of mean and sd (standard deviation) for fixed effects. Does not include intercept, time, or 
#'   interactions. Must be same order as fixed formula above.
#' @param cor_vars A vector of correlations between variables.
#' @param fact_vars A nested list of factor, categorical, or ordinal variable specification, 
#'      each list must include numlevels and var_type (must be "single");
#'      optional specifications are: replace, prob, value.labels.
#' @param contrasts An optional list that specifies the contrasts to be used for factor
#'      variables (i.e. those variables with .f or .c). See \code{\link{contrasts}} for 
#'      more detail.
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
  fact.loc <- grep("\\.f|\\.o|\\.c", fixed_vars, ignore.case = TRUE)  
  n.fact <- length(fact.loc[fact.loc != int.loc])
  n.cont <- length(cov_param$mean)
  
  cov_mu <- cov_param$mean
  cov_sd <- cov_param$sd
  
  if(length(fact.loc)> 0){
    fixed_vars <- c(fixed_vars[-c(fact.loc, int.loc)], fixed_vars[fact.loc], fixed_vars[int.loc])
  }
  
  if(n.fact > 0){
    if(!any(grepl("single", fact_vars$var_type))){
      stop("All variables must have var_type = 'single'")
    }
  }
  if(!is.null(cov_param)) {
    if(is.null(cor_vars)) {
      cov_param <- lapply(1:n.cont, function(xx) 
        list(k = 0, n = n, p = 0, mean = cov_param$mean[xx], sd = cov_param$sd[xx], 
             var_type = cov_param$var_type[xx]))
    } else {
      cov_param <- lapply(1:n.cont, function(xx) 
        list(k = 0, n = n, p = 0, mean = 0, sd = 1, 
             var_type = cov_param$var_type[xx]))
    }
    Xmat <- do.call("cbind", lapply(1:n.cont, function(xx) 
      do.call(sim_continuous, cov_param[[xx]])))
    
    if(!is.null(cor_vars)) {
      c_mat <- matrix(nrow = n.cont, ncol = n.cont)
      diag(c_mat) <- 1
      c_mat[upper.tri(c_mat)] <- c_mat[lower.tri(c_mat)] <- cor_vars
      cov <- diag(cov_sd) %*% c_mat %*% diag(cov_sd)
      es <- eigen(cov, symmetric = TRUE)
      ev <- es$values
      Xmat <- t(cov_mu + es$vectors %*% diag(sqrt(pmax(ev, 0)), length(cov_sd)) %*% 
        t(Xmat))
    }
  } else {
    Xmat <- NULL
  }

  if(length(fact.loc > 0)){
    #op <- names(fact_vars)
    fact_vars2 <- lapply(1:n.fact, function(xx) 
      list(k = 0, n = n, p = 0, numlevels = fact_vars$numlevels[xx], 
           var_type = fact_vars$var_type[xx]))
    Xmat <- cbind(Xmat, do.call("cbind", lapply(1:n.fact, 
            function(xx) do.call(sim_factor, fact_vars2[[xx]]))))
  }
  
  if(n.int == 0){
    colnames(Xmat) <- fixed_vars
  } else {
    int.loc <- grep(":", fixed_vars)
    colnames(Xmat) <- fixed_vars[-int.loc]
  } 
  if(any(grepl("\\.f|\\.c", fixed_vars, ignore.case = TRUE))) {
    fixed <- search_factors(fixed_vars)
  }
  Xmat <- model.matrix(fixed, data.frame(Xmat), contrasts.arg = contrasts)
  Xmat
}

#' Simulate categorical, factor, or discrete variables
#' 
#' Function that simulates discrete, factor, or categorical variables.  Is essentially
#' a wrapper around the sample function from base R.
#' 
#' @param k Number of third level clusters.
#' @param n Number of clusters or number of observations for single level
#' @param p Number of within cluster observations for multilevel
#' @param numlevels Scalar indicating the number of levels for categorical, factor, or discrete variable
#' @param replace Whether to replace levels of categorical variable, TRUE/FALSE
#' @param prob Probability of levels for variable, must be same length as numlevels
#' @param var_type Variable type for the variable, must be either "lvl1", "lvl2", or "single"
#' @param value.labels Optional argument with value labels for variable, 
#'        converts variable to factor.
#' @export 
sim_factor <- function(k = NULL, n, p, numlevels, replace = TRUE, prob = NULL, 
                       var_type = c('lvl1', 'lvl2', 'lvl3', 'single'), 
                       value.labels = NULL) {
  
  #if(is.null(prob) == FALSE & (length(prob) == numlevels | length(prob) == length(numlevels)) == FALSE) {
  #  stop("prob must be same length as numlevels")
  #}
  if(var_type == 'single' | var_type == 'lvl2') {
    if(replace == FALSE & numlevels < n) {
      stop("If replace = FALSE, numlevels must be greater than n for lvl2 or single")
    }
  }
  if(var_type == "lvl1") {
    if(replace == FALSE & numlevels < n*p){
      stop("If replace = FALSE, numlevels must be greater than n*p for lvl1")
    }
  }
  if(var_type == "lvl3") {
    if(replace == FALSE & numlevels < k) {
      stop("If replace = FALSE, numlevels must be greater than k for lvl3")
    }
  }
  
  end <- cumsum(n)
  beg <- c(1, cumsum(n) + 1)
  beg <- beg[-length(beg)]
  
  if(!is.null(k)) {
    lvl3ss <- sapply(lapply(1:length(beg), function(xx) 		
      p[beg[xx]:end[xx]]), sum)
  }
  
  var_type <- match.arg(var_type)
  
  catVar <- switch(var_type,
         single = sample(x = numlevels, size = n, replace = replace, 
                         prob = prob),
         lvl3 = rep(sample(x = numlevels, size = k, replace = replace, 
                           prob = prob), times = lvl3ss),
         lvl2 = rep(sample(x = numlevels, size = n, replace = replace, 
                           prob = prob), times = p),
         lvl1 = sample(x = numlevels, size = n*p, replace = replace, 
                       prob = prob)
         )
  
  if(!is.null(value.labels)) {
    if(length(value.labels) != numlevels) { 
      stop("value.labels must be same length as numlevels") 
      }
    catVar <- factor(catVar, labels = value.labels)
  }
  
  return(catVar)
}

#' Simulate continuous variables
#' 
#' Function that simulates continuous variables. Currently only supports
#' rnorm variables, future distributions to come.
#' 
#' @param k Number of third level clusters.
#' @param n Number of clusters or number of observations for single level
#' @param p Number of within cluster observations for multilevel
#' @param mean Mean for variable simulated from normal distribution
#' @param sd Standard deviation for variable simulated from normal distribution
#' @param var_type Variable type for the variable, must be either "lvl1", 
#'      "lvl2", or "single"
#' @export 
sim_continuous <- function(k = NULL, n, p, mean, sd, 
                           var_type = c('lvl1', 'lvl2', 'lvl3', 'single')) {
  
  end <- cumsum(n)
  beg <- c(1, cumsum(n) + 1)
  beg <- beg[-length(beg)]
  
  if(!is.null(k)) {
    lvl3ss <- sapply(lapply(1:length(beg), function(xx) 		
      p[beg[xx]:end[xx]]), sum)
  }
  
  var_type <- match.arg(var_type)
  
  contVar <- switch(var_type,
                   single = rnorm(n = n, mean = mean, sd = sd),
                   lvl3 = rep(rnorm(n = k, mean = mean, sd = sd), 
                              times = lvl3ss),
                   lvl2 = rep(rnorm(n = length(p), mean = mean, sd = sd), 
                              times = p),
                   lvl1 = rnorm(n = sum(p), mean = mean, sd = sd)
  )
  return(contVar)
}

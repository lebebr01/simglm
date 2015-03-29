#' Simulates design matrix.
#' 
#' Input fixed variables, sample size, and number of within variables, returns design matrix.
#' 
#' Simulates the fixed effects for the \code{\link{sim.reg}} function when a linear mixed
#' model is specified.  This function assumes a time variable when longitudinal data 
#' is specified and does include any interactions that are specified.
#' 
#' @param fixed One sided formula for fixed effects in the simulation.
#' @param fixed.vars Character vector of covariates for design matrix.
#' @param cov.param List of mean, sd (standard deviations), and var.type for fixed effects. 
#'  Does not include intercept, time, factors, or interactions. 
#'  var.type must be either "lvl1" or "lvl2". Must be same order as fixed formula above.
#' @param n Number of clusters.
#' @param p Number of within cluster units.
#' @param data.str Type of data. Must be "cross", or "long".
#' @param fact.vars A nested list of factor, categorical, or ordinal variable specification, 
#'      each list must include numlevels and var.type (must be "lvl1" or "lvl2");
#'      optional specifications are: replace, prob, value.labels.
#' @export 
sim.fixef.nested <- function(fixed, fixed.vars, cov.param, n, p, data.str, 
                             fact.vars = list(NULL)){
  
  n.vars <- length(fixed.vars)
  n.int <- length(grep(":",fixed.vars))
  int.loc <- grep(":", fixed.vars)
  fact.loc <- grep("\\.f|\\.o|\\.c", fixed.vars, ignore.case = TRUE) 
  w.var <- length(grep("lvl1", cov.param$var.type, ignore.case = TRUE))
  n.cont <- length(cov.param$mean)
  
  if(length(fact.loc) > 0){
    fixed.vars <- c(fixed.vars[-c(fact.loc, int.loc)], fixed.vars[fact.loc], fixed.vars[int.loc])
  }
  
  if(length(fact.loc) > 0){
    n.fact <- ifelse(length(int.loc) > 0, length(fact.loc[fact.loc != int.loc]), 
                     length(fact.loc))
    n.fact.lvl1 <- length(grep("lvl1", fact.vars$var.type, ignore.case = TRUE))
    n.fact.lvl2 <- length(grep("lvl2", fact.vars$var.type, ignore.case = TRUE))
  } else {
    n.fact <- 0
  } 

  if(data.str == "long") {
    Xmat <- rep.int((1:p) - 1, times = n)
    cov.param2 <- lapply(1:n.cont, function(xx) 
      list(k = 0, n = n, p = p, mean = cov.param$mean[xx], sd = cov.param$sd[xx], 
           var.type = cov.param$var.type[xx]))
    Xmat <- cbind(Xmat, do.call("cbind", lapply(1:n.cont, function(xx) 
      do.call(sim.continuous, cov.param2[[xx]]))))
  } else {
    cov.param2 <- lapply(1:n.cont, function(xx) 
      list(k = 0, n = n, p = p, mean = cov.param$mean[xx], sd = cov.param$sd[xx], 
           var.type = cov.param$var.type[xx]))
    Xmat <- do.call("cbind", lapply(1:n.cont, function(xx) 
      do.call(sim.continuous, cov.param2[[xx]])))
  }
  
  if(length(fact.loc > 0)){
    fact.vars <- lapply(1:length(fact.vars), function(xx) 
      list(k = 0, n = n, p = p, numlevels = fact.vars$numlevels[xx], 
           var.type = fact.vars$var.type[xx]))
    Xmat <- cbind(Xmat, do.call("cbind", lapply(1:n.fact, 
              function(xx) do.call(sim.factor, fact.vars[[xx]]))))
  }

   if(n.int == 0){
     colnames(Xmat) <- fixed.vars
   } else {
     int.loc <- grep(":", fixed.vars)
     colnames(Xmat) <- fixed.vars[-int.loc]
   } 
 Xmat <- model.matrix(fixed, data.frame(Xmat))
 Xmat
}

#' Simulates design matrix.
#' 
#' Input fixed variables, sample size, and number of within variables, returns design matrix.
#' 
#' Simulates the fixed effects for the \code{\link{sim.reg}} function when a linear mixed
#' model is specified.  This function assumes a time variable when longitudinal data 
#' is specified and does include any interactions that are specified.
#' 
#' @param fixed One sided formula for fixed effects in the simulation.
#' @param fixed.vars Character vector of covariates for design matrix.
#' @param cov.param List of mean, sd (standard deviations), and var.type for fixed effects. 
#'  Does not include intercept, time, factors, or interactions. 
#'  var.type must be either "lvl1", "lvl2", or "lvl3"". 
#'  Must be same order as fixed formula above.
#' @param k Number of third level clusters.
#' @param n Number of clusters.
#' @param p Number of within cluster units.
#' @param data.str Type of data. Must be "cross", or "long".
#' @param fact.vars A nested list of factor, categorical, or ordinal variable specification, 
#'      each list must include numlevels and var.type (must be "lvl1", "lvl2", or "lvl3");
#'      optional specifications are: replace, prob, value.labels.
#' @export 
sim.fixef.nested3 <- function(fixed, fixed.vars, cov.param, k, n, p, data.str, 
                             fact.vars = list(NULL)){
  
  n.vars <- length(fixed.vars)
  n.int <- length(grep(":",fixed.vars))
  int.loc <- grep(":", fixed.vars)
  fact.loc <- grep("\\.f|\\.o|\\.c", fixed.vars, ignore.case = TRUE) 
  n.cont <- length(cov.param$mean)
  
  if(length(fact.loc) > 0){
    fixed.vars <- c(fixed.vars[-c(fact.loc, int.loc)], fixed.vars[fact.loc], fixed.vars[int.loc])
  }
  
  if(length(fact.loc) > 0){
    n.fact <- ifelse(length(int.loc) > 0, length(fact.loc[fact.loc != int.loc]), 
                     length(fact.loc))
  } else {
    n.fact <- 0
  } 
  
 if(data.str == "long") {
    Xmat <- unlist(lapply(1:length(lvl1ss), function(xx) (1:lvl1ss[xx]) - 1))
    #Xmat <- rep.int((1:p) - 1, times = n)
    cov.param2 <- lapply(1:n.cont, function(xx) 
      list(k = k, n = n, p = p, mean = cov.param$mean[xx], sd = cov.param$sd[xx], 
           var.type = cov.param$var.type[xx]))
    Xmat <- cbind(Xmat, do.call("cbind", lapply(1:n.cont, function(xx) 
      do.call(sim.continuous, cov.param2[[xx]]))))
  } else {
    cov.param2 <- lapply(1:n.cont, function(xx) 
      list(k = k, n = n, p = p, mean = cov.param$mean[xx], sd = cov.param$sd[xx], 
           var.type = cov.param$var.type[xx]))
    Xmat <- do.call("cbind", lapply(1:n.cont, function(xx) 
      do.call(sim.continuous, cov.param2[[xx]])))
  }
  
  if(length(fact.loc > 0)){
    fact.vars <- lapply(1:length(fact.vars), function(xx) 
      list(k = k, n = n, p = p, numlevels = fact.vars$numlevels[xx], 
           var.type = fact.vars$var.type[xx]))
    Xmat <- cbind(Xmat, do.call("cbind", lapply(1:n.fact, 
                function(xx) do.call(sim.factor, fact.vars[[xx]]))))
  }
  
  if(n.int == 0){
    colnames(Xmat) <- fixed.vars
  } else {
    int.loc <- grep(":", fixed.vars)
    colnames(Xmat) <- fixed.vars[-int.loc]
  } 
  Xmat <- model.matrix(fixed, data.frame(Xmat))
  Xmat
}


#' Simulates design matrix for single level model.
#' 
#' Input fixed variables, sample size, and number of within variables, returns design matrix.
#' 
#' Simulates the fixed effects for the \code{\link{sim.reg}} function when simulating a 
#' simple regression model.
#' 
#' @param fixed One sided formula for fixed effects in the simulation.
#' @param fixed.vars Character vector of covariates for design matrix.
#' @param n Number of clusters.
#' @param cov.param List of mean and sd (standard deviation) for fixed effects. Does not include intercept, time, or 
#'   interactions. Must be same order as fixed formula above.
#' @param fact.vars A nested list of factor, categorical, or ordinal variable specification, 
#'      each list must include numlevels and var.type (must be "lvl1" or "lvl2");
#'      optional specifications are: replace, prob, value.labels.
#' @export 
sim.fixef.single <- function(fixed, fixed.vars, n, cov.param, fact.vars = list(NULL)){
  
  n.vars <- length(fixed.vars)
  n.int <- length(grep(":",fixed.vars))
  int.loc <- grep(":", fixed.vars)
  fact.loc <- grep("\\.f|\\.o|\\.c", fixed.vars, ignore.case = TRUE)  
  n.fact <- length(fact.loc[fact.loc != int.loc])
  n.cont <- length(cov.param$mean)
  
  if(length(fact.loc)> 0){
    fixed.vars <- c(fixed.vars[-c(fact.loc, int.loc)], fixed.vars[fact.loc], fixed.vars[int.loc])
  }
  
  if(n.fact > 0){
    if(all(grepl("single", fact.vars$var.type)) == FALSE){
      stop("All variables must have var.type = 'single'")
    }
  }
  
  cov.param <- lapply(1:n.cont, function(xx) 
    list(k = 0, n = n, p = 0, mean = cov.param$mean[xx], sd = cov.param$sd[xx], 
         var.type = cov.param$var.type[xx]))
  Xmat <- do.call("cbind", lapply(1:n.cont, function(xx) 
    do.call(sim.continuous, cov.param[[xx]])))
  
  if(length(fact.loc > 0)){
    #op <- names(fact.vars)
    fact.vars <- lapply(1:n.fact, function(xx) 
      list(k = 0, n = n, p = 0, numlevels = fact.vars$numlevels[xx], 
           var.type = fact.vars$var.type[xx]))
    Xmat <- cbind(Xmat, do.call("cbind", lapply(1:n.fact, 
            function(xx) do.call(sim.factor, fact.vars[[xx]]))))
  }
  
  if(n.int == 0){
    colnames(Xmat) <- fixed.vars
  } else {
    int.loc <- grep(":", fixed.vars)
    colnames(Xmat) <- fixed.vars[-int.loc]
  } 
  Xmat <- model.matrix(fixed, data.frame(Xmat))
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
#' @param var.type Variable type for the variable, must be either "lvl1", "lvl2", or "single"
#' @param value.labels Optional argument with value labels for variable, 
#'        converts variable to factor.
#' @export 
sim.factor <- function(k, n, p, numlevels, replace = TRUE, prob = NULL, var.type = c('lvl1', 'lvl2', 'lvl3', 'single'), 
                       value.labels = NULL) {
  
  #if(is.null(prob) == FALSE & (length(prob) == numlevels | length(prob) == length(numlevels)) == FALSE) {
  #  stop("prob must be same length as numlevels")
  #}
  if(replace == FALSE & (var.type == "single" | var.type == "lvl2") & numlevels < n) {
    stop("If replace = FALSE, numlevels must be greater than n for lvl2 or single")
  }
  if(var.type == "lvl1") {
    if(replace == FALSE & numlevels < n*p){
      stop("If replace = FALSE, numlevels must be greater than n*p for lvl1")
    }
  }
  if(replace == FALSE & var.type == "lvl3" & numlevels < k) {
    stop("If replace = FALSE, numlevels must be greater than k for lvl3")
  }
  
  var.type <- match.arg(var.type)
  
  catVar <- switch(var.type,
         single = sample(x = numlevels, size = n, replace = replace, prob = prob),
         lvl3 = rep(sampl(x = numlevels, size = k, replace = replace, prob = prob), each = (n*p)/k),
         lvl2 = rep(sample(x = numlevels, size = n, replace = replace, prob = prob), each = p),
         lvl1 = sample(x = numlevels, size = n*p, replace = replace, prob = prob)
         )
  
  if(is.null(value.labels) == FALSE) {
    if(length(value.labels) != numlevels) { stop("value.labels must be same length as numlevels") }
    catVar <- factor(catVar, labels = value.labels)
  }
  
  return(catVar)
}

#' Simulate categorical, factor, or discrete variables
#' 
#' Function that simulates discrete, factor, or categorical variables.  Is essentially
#' a wrapper around the sample function from base R.
#' 
#' @param k Number of third level clusters.
#' @param n Number of clusters or number of observations for single level
#' @param p Number of within cluster observations for multilevel
#' @param mean Mean for variable simulated from normal distribution
#' @param sd Standard deviation for variable simulated from normal distribution
#' @param var.type Variable type for the variable, must be either "lvl1", "lvl2", or "single"
#' @export 
sim.continuous <- function(k, n, p, mean, sd, var.type = c('lvl1', 'lvl2', 'lvl3', 'single')) {
  
  #if(is.null(prob) == FALSE & (length(prob) == numlevels | length(prob) == length(numlevels)) == FALSE) {
  #  stop("prob must be same length as numlevels")
  #}
  var.type <- match.arg(var.type)
  
  contVar <- switch(var.type,
                   single = rnorm(n = n, mean = mean, sd = sd),
                   lvl3 = rep(rnorm(n = k, mean = mean, sd = sd), times = sum(p)/k),
                   lvl2 = rep(rnorm(n = length(p), mean = mean, sd = sd), times = p),
                   lvl1 = rnorm(n = sum(p), mean = mean, sd = sd)
  )

  
  return(contVar)
}

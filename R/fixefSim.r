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
#' @param cov.param List of mean and variance for fixed effects. Does not include intercept, time, or 
#' interactions. Must be same order as fixed formula above.
#' @param n Number of clusters.
#' @param p Number of within cluster units.
#' @param w.var Number of time varying covariates or level one covariates for cross-sectional clustering.  
#' This number includes the intercept and time variable for longitudinal data.
#' @param data.str Type of data. Must be "cross", or "long".
#' @param fact.vars A list of factor, categorical, or ordinal variable specification, list must include
#'      numlevels, optional specifications are: replace, prob, value.labels.
#' @export 
sim.fixef.nested <- function(fixed, fixed.vars, cov.param, n, p, w.var, data.str, fact.vars = list(NULL)){
  
  n.vars <- length(fixed.vars)
  n.int <- length(grep(":",fixed.vars))
  int.loc <- grep(":", fixed.vars)
  fact.loc <- grep("\\.f|\\.o|\\.c", fixed.vars, ignore.case = TRUE) 
  
  Xmat <- matrix(nrow=n*p,ncol = ifelse(w.var == 1, 0, 1))

  if(data.str == "long"){
    if(w.var == 2){
      Xmat[,1] <- rep.int((1:p)-1,times = n)
    } else { 
      Xmat[,1] <- rep.int((1:p)-1,times = n)
      Xmat <- cbind(Xmat, do.call("cbind", lapply(3:w.var, function(xx) 
        rnorm(n * p, mean = cov.param[[xx-2]][1], sd = cov.param[[xx-2]][2]))))
      }
    } else {
       Xmat <- do.call("cbind", lapply(2:w.var, function(xx) 
         rnorm(n * p, mean = cov.param[[xx-1]][1], sd = cov.param[[xx-1]][2])))
     }
  
  
  if(n.int == 0){
    if(w.var != n.vars+1){
      Xmat <- cbind(Xmat, do.call("cbind", lapply((w.var+1):(n.vars+1), function(xx)
        rep(rnorm(n, mean = cov.param[[xx-2]][1], sd=cov.param[[xx-2]][2]), each = p))))
    } 
  } else {
    num.no.int <- n.vars - n.int                  
    if(w.var != num.no.int+1){
      Xmat <- cbind(Xmat, do.call("cbind", lapply((w.var+1):(num.no.int+1), function(xx)
        rep(rnorm(n, mean = cov.param[[xx-2]][1], sd=cov.param[[xx-2]][2]), each = p))))
    }
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
#' @param cov.param List of mean and variance for fixed effects. Does not include intercept, time, or 
#' interactions. Must be same order as fixed formula above.
#' @param fact.vars A list of factor, categorical, or ordinal variable specification, list must include
#'      numlevels, optional specifications are: replace, prob, value.labels.
#' @export 
sim.fixef.single <- function(fixed, fixed.vars, n, cov.param, fact.vars = list(NULL)){
  
  n.vars <- length(fixed.vars)
  n.int <- length(grep(":",fixed.vars))
  int.loc <- grep(":", fixed.vars)
  fact.loc <- grep("\\.f|\\.o|\\.c", fixed.vars, ignore.case = TRUE)  
  
  Xmat <- do.call("cbind", lapply(2:((n.vars - n.int)+1), function(xx)
    rnorm(n, mean = cov.param[[xx-1]][1], sd = cov.param[[xx-1]][2])))
  
  if(length(fact.loc > 0)){
    Xmat[, fact.loc[fact.loc != int.loc]] <- do.call("cbind", lapply(fact.loc[fact.loc != int.loc], 
            function(xx) sim.factor(n, numlevels = fact.vars$numlevels, data.str = "single")))
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
#' @param n Number of clusters or number of observations for single level
#' @param p Number of within cluster observations for multilevel
#' @param numlevels Scalar indicating the number of levels for categorical, factor, or discrete variable
#' @param replace Whether to replace levels of categorical variable, TRUE/FALSE
#' @param prob Probability of levels for variable, must be same length as numlevels
#' @param data.str Data structure for the data
#' @param value.labels Optional argument with value labels for variable, 
#'        converts variable to factor.
sim.factor <- function(n, p, numlevels, replace = TRUE, prob = NULL, data.str = c('lvl1', 'lvl2', 'single'), 
                       value.labels = NULL) {
  
  if(is.null(prob) == FALSE & (length(prob) == numlevels | length(prob) == length(numlevels)) == FALSE) {
    stop("prob must be same length as numlevels")
  }
  if(replace == FALSE & (data.str == "single" | data.str == "long") & numlevels < n) {
    stop("If replace = FALSE, numlevels must be greater than n")
  }
  if(data.str == "cross") {
    if(replace == FALSE & numlevels < n*p){
      stop("If replace = FALSE, numlevels must be greater than n*p")
    }
  }  
  
  data.str <- match.arg(data.str)
  
  catVar <- switch(data.str,
         single = sample(x = numlevels, size = n, replace = replace, prob = prob),
         lvl2 = rep(sample(x = numlevels, size = n, replace = replace, prob = prob), each = p),
         lvl1 = sample(x = numlevels, size = n*p, replace = replace, prob = prob)
         )
  
  if(is.null(value.labels) == FALSE) {
    if(length(value.labels) != numlevels) { stop("value.labels must be same length as numlevels") }
    catVar <- factor(catVar, labels = value.labels)
  }
  
  return(catVar)
}

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
#' @param fact.vars A list of factor, categorical, or ordinal variable specification, list must include
#'      numlevels and var.type (must be "lvl1" or "lvl2");
#'      optional specifications are: replace, prob, value.labels.
#' @export 
sim.fixef.nested <- function(fixed, fixed.vars, cov.param, n, p, data.str, 
                             fact.vars = list(NULL)){
  
  n.vars <- length(fixed.vars)
  n.int <- length(grep(":",fixed.vars))
  int.loc <- grep(":", fixed.vars)
  fact.loc <- grep("\\.f|\\.o|\\.c", fixed.vars, ignore.case = TRUE) 
  w.var <- length(grep("lvl1", cov.param$var.type, ignore.case = TRUE))
  
  if(length(fact.loc)> 0){
    fixed.vars <- c(fixed.vars[-c(fact.loc, int.loc)], fixed.vars[fact.loc], fixed.vars[int.loc])
  }
  
  if(length(fact.loc) > 0){
    n.fact <- length(fact.loc[fact.loc != int.loc])
    n.fact.lvl1 <- length(grep("lvl1", fact.vars$var.type, ignore.case = TRUE))
    n.fact.lvl2 <- length(grep("lvl2", fact.vars$var.type, ignore.case = TRUE))
  } else {
    n.fact <- 0
  } 

  if(data.str == "long"){
    w.var <- w.var + 1
    if(w.var == 1){
      Xmat <- rep.int((1:p)-1,times = n)
    } else { 
      Xmat <- rep.int((1:p)-1,times = n)
      Xmat <- cbind(Xmat, do.call("cbind", lapply(w.var, function(xx) 
        rnorm(n * p, mean = cov.param$mean[xx], sd = cov.param$sd[xx]))))
      }
    } else {
       Xmat <- do.call("cbind", lapply(w.var, function(xx) 
         rnorm(n * p, mean = cov.param$mean[xx], sd = cov.param$sd[xx])))
     }
  
  
  if(n.int == 0){
    if(w.var + n.fact != n.vars+1){
      Xmat <- cbind(Xmat, do.call("cbind", lapply((w.var):(n.vars-n.fact), function(xx)
        rep(rnorm(n, mean = cov.param$mean[xx], sd=cov.param$sd[xx]), each = p))))
    } 
  } else {
    num.no.int <- n.vars - n.int                  
    if(w.var + n.fact != num.no.int+1){
      Xmat <- cbind(Xmat, do.call("cbind", lapply((w.var):(num.no.int-1-n.fact), function(xx)
        rep(rnorm(n, mean = cov.param$mean[xx], sd=cov.param$sd[xx]), each = p))))
    }
  }  
  
  if(length(fact.loc > 0)){
    Xmat <- cbind(Xmat, do.call("cbind", lapply(n.fact, 
              function(xx) sim.factor(n, p, numlevels = fact.vars$numlevels[xx], 
                              var.type = fact.vars$var.type[xx]))))
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
#' @param fact.vars A list of factor, categorical, or ordinal variable specification, list must include
#'      numlevels and var.type (must be "single" for single level regression); 
#'      optional specifications are: replace, prob, value.labels.
#' @export 
sim.fixef.single <- function(fixed, fixed.vars, n, cov.param, fact.vars = list(NULL)){
  
  n.vars <- length(fixed.vars)
  n.int <- length(grep(":",fixed.vars))
  int.loc <- grep(":", fixed.vars)
  fact.loc <- grep("\\.f|\\.o|\\.c", fixed.vars, ignore.case = TRUE)  
  n.fact <- length(fact.loc[fact.loc != int.loc])
  
  if(length(fact.loc)> 0){
    fixed.vars <- c(fixed.vars[-c(fact.loc, int.loc)], fixed.vars[fact.loc], fixed.vars[int.loc])
  }
  
  if(n.fact > 0){
    if(all(grepl("single", fact.vars$var.type)) == FALSE){
      stop("All variables must have var.type = 'single'")
    }
  }
  
  Xmat <- do.call("cbind", lapply(1:((n.vars - n.int - n.fact)), function(xx)
    rnorm(n, mean = cov.param$mean[xx], sd = cov.param$sd[xx])))
  
  if(length(fact.loc > 0)){
    Xmat <- cbind(Xmat, do.call("cbind", lapply(n.fact, 
            function(xx) sim.factor(n, numlevels = fact.vars$numlevels[xx], 
                                    var.type = fact.vars$var.type[xx]))))
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
#' @param var.type Variable type for the variable, must be either "lvl1", "lvl2", or "single"
#' @param value.labels Optional argument with value labels for variable, 
#'        converts variable to factor.
#' @export 
sim.factor <- function(n, p, numlevels, replace = TRUE, prob = NULL, var.type = c('lvl1', 'lvl2', 'single'), 
                       value.labels = NULL) {
  
  if(is.null(prob) == FALSE & (length(prob) == numlevels | length(prob) == length(numlevels)) == FALSE) {
    stop("prob must be same length as numlevels")
  }
  if(replace == FALSE & (var.type == "single" | var.type == "lvl2") & numlevels < n) {
    stop("If replace = FALSE, numlevels must be greater than n")
  }
  if(var.type == "lvl1") {
    if(replace == FALSE & numlevels < n*p){
      stop("If replace = FALSE, numlevels must be greater than n*p")
    }
  }  
  
  var.type <- match.arg(var.type)
  
  catVar <- switch(var.type,
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

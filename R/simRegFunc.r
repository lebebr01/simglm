#' Function to simulate nested data
#' 
#' Takes simulation parameters as inputs and returns simulated data.
#' 
#' Simulates data for the linear mixed model, both cross sectional and longitudinal data.  Returns
#' a data frame with ID variables, fixed effects, and many other variables useful to help when 
#' running simulation studies.
#' 
#' @seealso \code{\link{sim.reg}} for a convenient wrapper for all data conditions.
#' 
#' @param fixed One sided formula for fixed effects in the simulation.  To suppress intercept add -1 to formula.
#' @param random One sided formula for random effects in the simulation. Must be a subset of fixed.
#' @param fixed.param Fixed effect parameter values (i.e. beta weights).  Must be same length as fixed.
#' @param random.param Variance of random effects. Must be same length as random.
#' @param cov.param List of mean, sd (standard deviations), and var.type for fixed effects. 
#'  Does not include intercept, time, factors, or interactions. 
#'  var.type must be either "lvl1" or "lvl2". Must be same order as fixed formula above.
#' @param n Cluster sample size.
#' @param p Within cluster sample size.
#' @param errorVar Scalar of error variance.
#' @param randCor Correlation between random effects.
#' @param rand.dist Simulated random effect distribution.  Must be "lap", "chi", "norm", "bimod", 
#' "norm" is default.
#' @param err.dist Simulated within cluster error distribution. Must be "lap", "chi", "norm", "bimod", 
#' "norm" is default.
#' @param serCor Simulation of serial correlation. Must be "AR", "MA", "ARMA", or "ID", "ID" is default.
#' @param serCorVal Serial correlation parameters. A list of values to pass on to arima.sim.
#' @param data.str Type of data. Must be "cross", "long", or "single".
#' @param fact.vars A nested list of factor, categorical, or ordinal variable specification, 
#'      each list must include numlevels and var.type (must be "lvl1" or "lvl2");
#'      optional specifications are: replace, prob, value.labels.
#' @param num.dist Number of distributions for bimod random distribution
#' @param ... Additional arguments to pass to rbimod 
#' @export 
sim.reg.nested <- function(fixed, random, fixed.param, random.param, cov.param, n, p, errorVar, randCor, 
                    rand.dist, err.dist, serCor, serCorVal, data.str, fact.vars = list(NULL), num.dist, ...) {

  if(randCor > 1 | randCor < -1) stop("cor out of range")

  fixed.vars <- attr(terms(fixed),"term.labels")    ##Extracting fixed effect term labels
  rand.vars <- attr(terms(random),"term.labels")   ##Extracting random effect term labels

     if(length(rand.vars)+1 != length(random.param)) stop("Random lengths not equal")
     if({length(fixed.vars)+1} != {length(fixed.param)}) stop("Fixed lengths not equal")

   rand.eff <- sim.rand.eff(random.param, randCor, n, rand.dist, num.dist)

   Xmat <- sim.fixef.nested(fixed, fixed.vars, cov.param, n, p, 
                            data.str, fact.vars)
  
  reff <- do.call("cbind", lapply(1:ncol(rand.eff), function(xx) 
    rep(rand.eff[,xx], each = p)))
  colnames(reff) <- c(unlist(lapply(1:ncol(rand.eff), function(xx) paste("b", xx-1, sep = ""))))
  
  Zmat <- model.matrix(random, data.frame(Xmat))

  err <- sim.err.nested(errorVar, n, p, serCor, serCorVal, err.dist, num.dist, mean, var)

 sim.data <- data.reg.nested(Xmat, Zmat, fixed.param, rand.eff, n, p, err)
  
 Xmat <- data.frame(Xmat,reff,sim.data)
 Xmat$withinID <- rep.int(1:p, n)
 Xmat$clustID <- rep(1:n, each = p)
 Xmat
}

#' Function to simulate three level nested data
#' 
#' Takes simulation parameters as inputs and returns simulated data.
#' 
#' Simulates data for the linear mixed model, both cross sectional and longitudinal data.  Returns
#' a data frame with ID variables, fixed effects, and many other variables useful to help when 
#' running simulation studies.
#' 
#' @seealso \code{\link{sim.reg}} for a convenient wrapper for all data conditions.
#' 
#' @param fixed One sided formula for fixed effects in the simulation.  To suppress intercept add -1 to formula.
#' @param random One sided formula for random effects in the simulation. Must be a subset of fixed.
#' @param random3 One sided formula for random effects at third level in the simulation. Must be a subset of fixed
#'  (and likely of random).
#' @param fixed.param Fixed effect parameter values (i.e. beta weights).  Must be same length as fixed.
#' @param random.param Variance of random effects. Must be same length as random.
#' @param random.param3 Variance of level 3 random effects. Must be same length as random3.
#' @param cov.param List of mean, sd (standard deviations), and var.type for fixed effects. 
#'  Does not include intercept, time, factors, or interactions. 
#'  var.type must be either "lvl1" or "lvl2". Must be same order as fixed formula above.
#' @param k Number of third level clusters.
#' @param n Cluster sample size.
#' @param p Within cluster sample size.
#' @param errorVar Scalar of error variance.
#' @param randCor Correlation between random effects.
#' @param randCor3 Correlation between level 3 random effects.
#' @param rand.dist Simulated random effect distribution.  Must be "lap", "chi", "norm", "bimod", 
#' "norm" is default.
#' @param err.dist Simulated within cluster error distribution. Must be "lap", "chi", "norm", "bimod", 
#' "norm" is default.
#' @param serCor Simulation of serial correlation. Must be "AR", "MA", "ARMA", or "ID", "ID" is default.
#' @param serCorVal Serial correlation parameters. A list of values to pass on to arima.sim.
#' @param data.str Type of data. Must be "cross", "long", or "single".
#' @param fact.vars A nested list of factor, categorical, or ordinal variable specification, 
#'      each list must include numlevels and var.type (must be "lvl1" or "lvl2");
#'      optional specifications are: replace, prob, value.labels.
#' @param num.dist Number of distributions for bimod random distribution
#' @param ... Additional arguments to pass to rbimod 
#' @export 
sim.reg.nested3 <- function(fixed, random, random3, fixed.param, random.param, random.param3, cov.param, k, n, p, 
                            errorVar, randCor, randCor3, rand.dist, err.dist, serCor, serCorVal, data.str, 
                            fact.vars = list(NULL), num.dist, ...) {

  if(randCor > 1 | randCor < -1 | randCor3 > 1 | randCor3 < -1) stop("Random effect correlation out of range")

  fixed.vars <- attr(terms(fixed),"term.labels")    ##Extracting fixed effect term labels
  rand.vars <- attr(terms(random),"term.labels")   ##Extracting random effect term labels
  rand.vars3 <- attr(terms(random3),"term.labels")   ##Extracting random effect term labels

     if(length(rand.vars)+1 != length(random.param)) stop("Random lengths not equal")
     if(length(rand.vars3)+1 != length(random.param3)) stop("Third level random lengths not equal")
     if({length(fixed.vars)+1} != {length(fixed.param)}) stop("Fixed lengths not equal")

   rand.eff <- sim.rand.eff(random.param, randCor, n, rand.dist, num.dist)
   rand.eff3 <- sim.rand.eff3(random.param3, randCor3, k)

   Xmat <- sim.fixef.nested3(fixed, fixed.vars, cov.param, k, n, p, data.str, fact.vars)
  
  reff <- do.call("cbind", lapply(1:ncol(rand.eff), function(xx) 
    rep(rand.eff[,xx], each = p)))
  colnames(reff) <- c(unlist(lapply(1:ncol(rand.eff), function(xx) paste("b", xx-1, "_2", sep = ""))))
  
  reff3 <- do.call("cbind", lapply(1:ncol(rand.eff3), function(xx) 
    rep(rand.eff3[,xx], each = (n*p)/k)))
  colnames(reff3) <- c(unlist(lapply(1:ncol(rand.eff3), function(xx) paste("b", xx-1, "_3", sep = ""))))
  
  Zmat <- model.matrix(random, data.frame(Xmat))
  Zmat3 <- model.matrix(random3, data.frame(Xmat))

  err <- sim.err.nested(errorVar, n, p, serCor, serCorVal, err.dist, num.dist, mean, var)

 sim.data <- data.reg.nested3(Xmat, Zmat, Zmat3, fixed.param, rand.eff, rand.eff3, k, n, p, err)
  
 Xmat <- data.frame(Xmat,reff,sim.data)
 Xmat$withinID <- rep.int(1:p, n)
 Xmat$clustID <- rep(1:n, each = p)
 Xmat$clust3ID <- rep(1:k, each = (n*p)/k)
 Xmat
}


#' Master function to simulate single level data.
#' 
#' Takes simulation parameters as inputs and returns simulated data.
#' 
#' Simulates data for the simple regression models.  Returns a data frame with ID variables, 
#' fixed effects, and many other variables useful to help when running simulation studies.
#' 
#' @seealso \code{\link{sim.reg}} for a convenient wrapper for all data conditions.
#' 
#' @param fixed One sided formula for fixed effects in the simulation.  To suppress intercept add -1 to formula.
#' @param fixed.param Fixed effect parameter values (i.e. beta weights).  Must be same length as fixed.
#' @param cov.param List of mean and sd (standard deviation) for fixed effects. Does not include intercept, time, or 
#'   interactions. Must be same order as fixed formula above.
#' @param n Cluster sample size.
#' @param errorVar Scalar of error variance.
#' @param err.dist Simulated within cluster error distribution. Must be "lap", "chi", "norm", "bimod", 
#'   "norm" is default.
#' @param data.str Type of data. Must be "cross", "long", or "single".
#' @param fact.vars A nested list of factor, categorical, or ordinal variable specification, 
#'      each list must include numlevels and var.type (must be "lvl1" or "lvl2");
#'      optional specifications are: replace, prob, value.labels.
#' @param num.dist Number of distributions for bimodal random variables
#' @param ... Additional arguments to pass to rbimod
#' @export 
sim.reg.single <- function(fixed, fixed.param, cov.param, n, errorVar, err.dist, data.str, 
                           fact.vars = list(NULL), num.dist, ...) {
  
  fixed.vars <- attr(terms(fixed),"term.labels")    ##Extracting fixed effect term labels
  
  if({length(fixed.vars)+1} != {length(fixed.param)}) stop("Fixed lengths not equal")
  
  Xmat <- sim.fixef.single(fixed, fixed.vars, n, cov.param, fact.vars)
  
  err <- sim.err.single(errorVar, n, err.dist, num.dist, ...)
  
  sim.data <- data.reg.single(Xmat, fixed.param, n, err)
  
  Xmat <- data.frame(Xmat,sim.data)
  Xmat$ID <- 1:n
  Xmat
}

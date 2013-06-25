#' Function to simulate nested data
#' 
#' Takes simulation parameters as inputs and 
#' returns simulated data.
#' 
#' @param fixed One sided formula for fixed effects in the simulation.  To suppress intercept add -1 to formula.
#' @param random One sided formula for random effects in the simulation. Must be a subset of fixed.
#' @param fixed.param Fixed effect parameter values (i.e. beta weights).  Must be same length as fixed.
#' @param random.param Variance of random effects. Must be same length as random.
#' @param w.var Number of within cluster variables, including intercept if applicable.   
#' Also could be number of level one covariates for cross-sectional clustering.
#' @param cov.param List of mean and variance for fixed effects. Does not include intercept, time, or 
#' interactions. Must be same order as fixed formula above.
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
#' @parm num.dist Number of distributions for bimod random distribution
#' @import MASS Matrix
#' @export 
sim.reg.nested <- function(fixed, random, fixed.param, random.param, w.var, cov.param, n, p, errorVar, randCor, 
                    rand.dist, err.dist, serCor, serCorVal, data.str, num.dist) {

  if(randCor > 1 | randCor < -1) stop("cor out of range")

  fixed.vars <- attr(terms(fixed),"term.labels")    ##Extracting fixed effect term labels
  rand.vars <- attr(terms(random),"term.labels")   ##Extracting random effect term labels

     if(length(rand.vars)+1 != length(random.param)) stop("Random lengths not equal")
     if({length(fixed.vars)+1} != {length(fixed.param)}) stop("Fixed lengths not equal")

   rand.eff <- rand.eff.sim(random.param, randCor, n, rand.dist, num.dist)

   Xmat <- fixef.sim.nested(fixed, fixed.vars, n, p, w.var, data.str)
  
  reff <- do.call("cbind", lapply(1:ncol(rand.eff), function(xx) 
    rep(rand.eff[,xx], each = p)))
  colnames(reff) <- c(unlist(lapply(1:ncol(rand.eff), function(xx) paste("b", xx-1, sep = ""))))
  
  Zmat <- model.matrix(random, data.frame(Xmat))

 #if(serCor == "AR" | serCor == "MA" | serCor == "ARMA" & is.list(serCorVal) == "FALSE") {stop("Incorrect dimensions serCorVal")}
  err <- err.sim.nested(errorVar, n, p, serCor, serCorVal, err.dist, num.dist)

 sim.data <- data.reg.nested(Xmat, Zmat, fixed.param, rand.eff, n, p, err)
  
 Xmat <- data.frame(Xmat,reff,sim.data)
 Xmat$withinID <- rep.int(1:p, n)
 Xmat$clustID <- rep(1:n, each = p)
 Xmat
}
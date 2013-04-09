#' Power simulation for nested designs
#' 
#' Takes simulation conditions as input, 
#' exports power.
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
#' @param pow.param Number of parameter to calculate power includes intercept where applicable.
#' @param alpha What should the per test alpha rate be used for the hypothesis testing.
#' @param pow.dist Which distribution should be used when testing hypothesis test, z or t?
#' @param pow.tail One-tailed or two-tailed test?
#' @export 
sim.pow.nested <- function(fixed, random, fixed.param, random.param, w.var, cov.param, n, p, errorVar, randCor, 
                    rand.dist, err.dist, serCor, serCorVal, data.str,
                           pow.param, alpha, pow.dist = c("z", "t"), pow.tail = c(1, 2)){
  
  #require(lme4)
  require(nlme)
  
  temp.nest <- sim.reg.nested(fixed, random, fixed.param, random.param, w.var, cov.param, n, p, 
                              errorVar, randCor, rand.dist, err.dist, serCor, serCorVal, data.str)
  
  fixed.vars <- attr(terms(fixed),"term.labels")    ##Extracting fixed effect term labels
  rand.vars <- attr(terms(random),"term.labels")
  
  fix1 <- paste("sim.data ~", paste(fixed.vars, collapse = "+"))
  #ran1 <- paste("(", paste(rand.vars, collapse = "+"), "|clustID)", sep = "")
  #fm1 <- as.formula(paste(fix1, ran1, sep = "+ "))
  ran1 <- paste("~", paste(rand.vars, collapse = "+"), "|clustID", sep = "")
  
  temp.lme <- lme(fixed = as.formula(fix1), data = temp.nest, random = as.formula(ran1))
  
  crit <- qnorm(alpha/pow.tail, lower.tail = FALSE)
  #coefTab <- coef.tbl(temp.lmer)
  #testStat <- coefTab[pow.param]
  testStat <- summary(temp.lme)$tTable[pow.param, 3]
  
  reject <- ifelse(testStat >= crit, 1, 0)
  
  return(reject)  
}
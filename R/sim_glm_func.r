#' Simulation single level logistic regression model
#' 
#' Takes simulation parameters as inputs and returns simulated data.
#' 
#' Simulates data for the simple logistic regression models.  Returns 
#' a data frame with ID variables, fixed effects, and many other variables
#' to help when running simulation studies.
#' 
#' @param fixed One sided formula for fixed effects in the simulation.  To suppress intercept add -1 to formula.
#' @param fixed.param Fixed effect parameter values (i.e. beta weights).  Must be same length as fixed.
#' @param cov.param List of mean and sd (standard deviation) for fixed effects. Does not include intercept, time, or 
#'   interactions. Must be same order as fixed formula above.
#' @param n Cluster sample size.
#' @param data_str Type of data. Must be "cross", "long", or "single".
#' @param cor_vars A vector of correlations between variables.
#' @param fact.vars A nested list of factor, categorical, or ordinal variable specification, 
#'      each list must include numlevels and var.type (must be "lvl1" or "lvl2");
#'      optional specifications are: replace, prob, value.labels.
#'             
#' @examples 
#' \donttest{
#' # generating parameters for single level logistic regression
#' fixed <- ~1 + act + diff + numCourse + act:numCourse
#' fixed.param <- c(0.2, 1.5, 0.8, 1.2, 1.1)
#' cov.param <- list(mean = c(0, 0, 0), sd = c(1, 1, 1), var.type = c("single", "single", "single"))
#' n <- 150
#' temp.single <- sim_glm_single(fixed = fixed, fixed.param = fixed.param, cov.param = cov.param, 
#' n = n)
#' # Fitting regression to obtain parameter estimates
#' summary(glm(sim.data ~ 1 + act + diff + numCourse + act:numCourse, data = temp.single,
#' family = "binomial"))
#' 
#' }
#' @export
sim_glm_single <- function(fixed, fixed.param, cov.param, n, 
                           data_str, cor_vars = NULL, fact.vars = list(NULL)) {
  
  fixed.vars <- attr(terms(fixed),"term.labels")    ##Extracting fixed effect term labels
  
  if({length(fixed.vars)+1} != {length(fixed.param)}) stop("Fixed lengths not equal")
  
  Xmat <- sim_fixef_single(fixed, fixed.vars, n, cov.param, cor_vars, fact.vars)
  
  sim.data <- data_glm_single(Xmat, fixed.param, n)
  
  Xmat <- data.frame(Xmat,sim.data)
  Xmat$ID <- 1:n
  return(Xmat)
  
}

#' Simulate two level logistic regression model
#' 
#' Takes simulation parameters as inputs and returns simulated data.
#' 
#' Simulates data for the nested logistic regression models.  Returns a 
#' data frame with ID variables, fixed effects, random effects, and many
#' other variables to help when running simulation studies.
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
#' @param randCor Correlation between random effects.
#' @param rand_dist Simulated random effect distribution.  Must be "lap", "chi", "norm", "bimod", 
#' "norm" is default.
#' @param data_str Type of data. Must be "cross", "long", or "single".
#' @param cor_vars A vector of correlations between variables.
#' @param fact.vars A nested list of factor, categorical, or ordinal variable specification, 
#'      each list must include numlevels and var.type (must be "lvl1" or "lvl2");
#'      optional specifications are: replace, prob, value.labels.
#' @param unbal A vector of sample sizes for the number of observations for each level 2
#'  cluster. Must have same length as level two sample size n. Alternative specification
#'  can be TRUE, which uses additional argument, unbalCont.
#' @param unbalCont When unbal = TRUE, this specifies the minimum and maximum level one size,
#'  will be drawn from a random uniform distribution with min and max specified.
#'
#' @examples
#' \donttest{
#' fixed <- ~1 + time + diff + act + time:act
#' random <- ~1
#' fixed.param <- c(0.2, 1.5, 0.8, 1.2, 1.1)
#' random.param <- c(3)
#' cov.param <- list(mean = c(0, 0), sd = c(1.5, 4), var.type = c("lvl1", "lvl2"))
#' n <- 100
#' p <- 10
#' randCor <- 0
#' rand_dist <- "norm"
#' data_str <- "long"
#' temp.long <- sim_glm_nested(fixed, random, fixed.param, random.param,
#'  cov.param, n, p, randCor, rand_dist, data_str = data_str)
#' }
#' @export
sim_glm_nested <- function(fixed, random, fixed.param, random.param, cov.param, n, p, 
                           randCor, rand_dist, 
                           data_str, cor_vars = NULL, fact.vars = list(NULL),
                           unbal = FALSE, unbalCont = NULL) {
  
  if(randCor > 1 | randCor < -1) stop("cor out of range")
  
  fixed.vars <- attr(terms(fixed),"term.labels")    ##Extracting fixed effect term labels
  rand.vars <- attr(terms(random),"term.labels")   ##Extracting random effect term labels
  
  if(length(rand.vars)+1 != length(random.param)) stop("Random lengths not equal")
  if({length(fixed.vars)+1} != {length(fixed.param)}) stop("Fixed lengths not equal")
  
  if(unbal == FALSE) {
    lvl1ss <- rep(p, n)
    if(is.null(lvl1ss)) stop("lvl1ss is NULL")
  } else {
    if(length(unbalCont) < 2) stop("Must specify unbalCont when unbal = TRUE")
    lvl1ss <- round(runif(n = n, min = min(unbalCont), max = max(unbalCont)), 0)
  }
  
  rand.eff <- sim_rand_eff(random.param, randCor, n, rand_dist)
  
  Xmat <- sim_fixef_nested(fixed, fixed.vars, cov.param, n, lvl1ss, 
                           data_str = data_str, cor_vars = cor_vars, fact.vars = fact.vars)
  
  reff <- do.call("cbind", lapply(1:ncol(rand.eff), function(xx) 
    rep(rand.eff[,xx], times = lvl1ss)))
  colnames(reff) <- c(unlist(lapply(1:ncol(rand.eff), function(xx) paste("b", xx-1, sep = ""))))
  
  Zmat <- model.matrix(random, data.frame(Xmat))

  sim.data <- data_glm_nested(Xmat, Zmat, fixed.param, rand.eff, n, p = lvl1ss)
  
  Xmat <- data.frame(Xmat,reff,sim.data)
  Xmat$withinID <- unlist(lapply(1:length(lvl1ss), function(xx) 1:lvl1ss[xx]))
  Xmat$clustID <- rep(1:n, times = lvl1ss)
  return(Xmat)
  
}

#' Function to simulate three level nested data
#' 
#' Takes simulation parameters as inputs and returns simulated data.
#' 
#' Simulates data for the linear mixed model, both cross sectional and longitudinal data.  Returns
#' a data frame with ID variables, fixed effects, and many other variables useful to help when 
#' running simulation studies.
#' 
#' @seealso \code{\link{sim_reg}} for a convenient wrapper for all data conditions.
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
#' @param randCor Correlation between random effects.
#' @param randCor3 Correlation between level 3 random effects.
#' @param rand_dist Simulated random effect distribution.  Must be "lap", "chi", "norm", "bimod", 
#' "norm" is default.
#' @param data_str Type of data. Must be "cross", "long", or "single".
#' @param cor_vars A vector of correlations between variables.
#' @param fact.vars A nested list of factor, categorical, or ordinal variable specification, 
#'      each list must include numlevels and var.type (must be "lvl1" or "lvl2");
#'      optional specifications are: replace, prob, value.labels.
#' @param unbal A vector of sample sizes for the number of observations for each level 2
#'  cluster. Must have same length as level two sample size n. Alternative specification
#'  can be TRUE, which uses additional argument, unbalCont.
#' @param unbal3 A vector of sample sizes for the number of observations for each level 3
#'  cluster. Must have same length as level two sample size k. Alternative specification
#'  can be TRUE, which uses additional argument, unbalCont3.
#' @param unbalCont When unbal = TRUE, this specifies the minimum and maximum level one size,
#'  will be drawn from a random uniform distribution with min and max specified.
#' @param unbalCont3 When unbal3 = TRUE, this specifies the minimum and maximum level two size,
#'  will be drawn from a random uniform distribution with min and max specified.
#' 
#' @examples 
#' \donttest{
#' # Three level example
#' fixed <- ~1 + time + diff + act + actClust + time:act
#' random <- ~1 + time + diff
#' random3 <- ~ 1 + time
#' fixed.param <- c(4, 2, 6, 2.3, 7, 0)
#' random.param <- c(7, 4, 2)
#' random.param3 <- c(4, 2)
#' cov.param <- list(mean = c(0, 0, 0), sd = c(1.5, 4, 2),
#' var.type = c("lvl1", "lvl2", "lvl3"))
#' k <- 10
#' n <- 150
#' p <- 30
#' randCor <- 0
#' randCor3 <- 0
#' rand_dist <- "norm"
#' data_str <- "long"
#' temp.three <- sim_glm_nested3(fixed, random, random3, fixed.param, random.param,
#' random.param3, cov.param, k, n, p, randCor, randCor3, rand_dist,
#' data_str = data_str)
#' head(temp.three)
#' }
#' @export 
sim_glm_nested3 <- function(fixed, random, random3, fixed.param, random.param, random.param3,
                            cov.param, k, n, p, randCor, randCor3, rand_dist,
                            data_str, cor_vars = NULL, fact.vars = list(NULL),
                            unbal = FALSE, unbal3 = FALSE, unbalCont = NULL, unbalCont3 = NULL) {
  
  if(randCor > 1 | randCor < -1 | randCor3 > 1 | randCor3 < -1) 
    stop("Random effect correlation out of range")
  
  fixed.vars <- attr(terms(fixed),"term.labels")    ##Extracting fixed effect term labels
  rand.vars <- attr(terms(random),"term.labels")   ##Extracting random effect term labels
  rand.vars3 <- attr(terms(random3),"term.labels")   ##Extracting random effect term labels
  
  if(length(rand.vars)+1 != length(random.param)) stop("Random lengths not equal")
  if(length(rand.vars3)+1 != length(random.param3)) stop("Third level random lengths not equal")
  if({length(fixed.vars)+1} != {length(fixed.param)}) stop("Fixed lengths not equal")
  
  if(unbal3 == FALSE) {
    lvl2ss <- rep(n/k, k)
    n <- sum(lvl2ss)
  } else {
    if(length(unbalCont3) < 2) stop("Must specify unbalCont3 when unbal3 = TRUE")
    lvl2ss <- round(runif(n = k, min = min(unbalCont3), max = max(unbalCont3)), 0)
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
  
  rand.eff <- sim_rand_eff(random.param, randCor, n, rand_dist)
  rand.eff3 <- sim_rand_eff3(random.param3, randCor3, k)
  
  Xmat <- sim_fixef_nested3(fixed, fixed.vars, cov.param, k, n = lvl2ss, 
                            p = lvl1ss, data_str = data_str, cor_vars = cor_vars, 
                            fact.vars = fact.vars)
  
  reff <- do.call("cbind", lapply(1:ncol(rand.eff), function(xx) 
    rep(rand.eff[,xx], times = lvl1ss)))
  colnames(reff) <- c(unlist(lapply(1:ncol(rand.eff), function(xx) paste("b", xx-1, "_2", sep = ""))))
  
  reff3 <- do.call("cbind", lapply(1:ncol(rand.eff3), function(xx) 
    rep(rand.eff3[,xx], times = lvl2ss)))
  colnames(reff3) <- c(unlist(lapply(1:ncol(rand.eff3), function(xx) paste("b", xx-1, "_3", sep = ""))))
  
  Zmat <- model.matrix(random, data.frame(Xmat))
  Zmat3 <- model.matrix(random3, data.frame(Xmat))
  
  sim.data <- data_glm_nested3(Xmat, Zmat, Zmat3, fixed.param, rand.eff, rand.eff3,
                               k, n = lvl2ss, p = lvl1ss)
  
  Xmat <- data.frame(Xmat, reff, sim.data)
  Xmat$withinID <- unlist(lapply(1:length(lvl1ss), function(xx) 1:lvl1ss[xx]))
  Xmat$clustID <- rep(1:n, times = lvl1ss)
  Xmat$clust3ID <- rep(1:k, times = lvl3ss)
  return(Xmat)
}



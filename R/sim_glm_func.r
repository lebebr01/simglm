#' Simulation single level logistic regression model
#' 
#' Takes simulation parameters as inputs and returns simulated data.
#' 
#' Simulates data for the simple logistic regression models.  Returns 
#' a data frame with ID variables, fixed effects, and many other variables
#' to help when running simulation studies.
#' 
#' @param fixed One sided formula for fixed effects in the simulation.  To suppress intercept add -1 to formula.
#' @param fixed_param Fixed effect parameter values (i.e. beta weights).  Must be same length as fixed.
#' @param cov_param List of mean and sd (standard deviation) for fixed effects. Does not include intercept, time, or 
#'   interactions. Must be same order as fixed formula above.
#' @param n Cluster sample size.
#' @param data_str Type of data. Must be "cross", "long", or "single".
#' @param cor_vars A vector of correlations between variables.
#' @param fact_vars A nested list of factor, categorical, or ordinal variable specification, 
#'      each list must include numlevels and var_type (must be "lvl1" or "lvl2");
#'      optional specifications are: replace, prob, value.labels.
#'             
#' @examples 
#' \donttest{
#' # generating parameters for single level logistic regression
#' fixed <- ~1 + act + diff + numCourse + act:numCourse
#' fixed_param <- c(0.2, 1.5, 0.8, 1.2, 1.1)
#' cov_param <- list(mean = c(0, 0, 0), sd = c(1, 1, 1), var_type = c("single", "single", "single"))
#' n <- 150
#' temp.single <- sim_glm_single(fixed = fixed, fixed_param = fixed_param, cov_param = cov_param, 
#' n = n)
#' # Fitting regression to obtain parameter estimates
#' summary(glm(sim_data ~ 1 + act + diff + numCourse + act:numCourse, data = temp.single,
#' family = "binomial"))
#' 
#' }
#' @export
sim_glm_single <- function(fixed, fixed_param, cov_param, n, 
                           data_str, cor_vars = NULL, fact_vars = list(NULL)) {
  
  fixed_vars <- attr(terms(fixed),"term.labels")    ##Extracting fixed effect term labels
  
  if({length(fixed_vars)+1} != {length(fixed_param)}) stop("Fixed lengths not equal")
  
  Xmat <- sim_fixef_single(fixed, fixed_vars, n, cov_param, cor_vars, fact_vars)
  
  sim_data <- data_glm_single(Xmat, fixed_param, n)
  
  Xmat <- data.frame(Xmat,sim_data)
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
#' @param fixed_param Fixed effect parameter values (i.e. beta weights).  Must be same length as fixed.
#' @param random_param A list of named elements that must contain: 
#'             random_var = variance of random parameters,
#'             rand_gen = Name of simulation function for random effects.
#'          Optional elements are:
#'             ther: Theorectial mean and variance from rand_gen,
#'             ther_sim: Simulate mean/variance for standardization purposes,
#'             cor_vars: Correlation between random effects,
#'             ...: Additional parameters needed for rand_gen function.
#' @param cov_param List of mean, sd (standard deviations), and var_type for fixed effects. 
#'  Does not include intercept, time, factors, or interactions. 
#'  var_type must be either "lvl1" or "lvl2". Must be same order as fixed formula above.
#' @param n Cluster sample size.
#' @param p Within cluster sample size.
#' @param data_str Type of data. Must be "cross", "long", or "single".
#' @param cor_vars A vector of correlations between variables.
#' @param fact_vars A nested list of factor, categorical, or ordinal variable specification, 
#'      each list must include numlevels and var_type (must be "lvl1" or "lvl2");
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
#' fixed_param <- c(0.2, 1.5, 0.8, 1.2, 1.1)
#' random_param <- list(random_var = 3, rand_gen = 'rnorm')
#' cov_param <- list(mean = c(0, 0), sd = c(1.5, 4), var_type = c("lvl1", "lvl2"))
#' n <- 100
#' p <- 10
#' data_str <- "long"
#' temp.long <- sim_glm_nested(fixed, random, fixed_param, random_param,
#'  cov_param, n, p, rand_dist, data_str = data_str)
#' }
#' @export
sim_glm_nested <- function(fixed, random, fixed_param, random_param = list(), cov_param, n, p, 
                           data_str, cor_vars = NULL, fact_vars = list(NULL),
                           unbal = FALSE, unbalCont = NULL) {
  
  #if(randCor > 1 | randCor < -1) stop("cor out of range")
  
  fixed_vars <- attr(terms(fixed),"term.labels")    ##Extracting fixed effect term labels
  rand.vars <- attr(terms(random),"term.labels")   ##Extracting random effect term labels
  
  if(length(rand.vars)+1 != length(random_param$random_var)) stop("Random lengths not equal")
  if({length(fixed_vars)+1} != {length(fixed_param)}) stop("Fixed lengths not equal")
  
  if(unbal == FALSE) {
    lvl1ss <- rep(p, n)
    if(is.null(lvl1ss)) stop("lvl1ss is NULL")
  } else {
    if(length(unbalCont) < 2) stop("Must specify unbalCont when unbal = TRUE")
    lvl1ss <- round(runif(n = n, min = min(unbalCont), max = max(unbalCont)), 0)
  }
  
  rand_eff <- do.call(sim_rand_eff, c(random_param, n = n))
  
  Xmat <- sim_fixef_nested(fixed, fixed_vars, cov_param, n, lvl1ss, 
                           data_str = data_str, cor_vars = cor_vars, fact_vars = fact_vars)
  
  reff <- do.call("cbind", lapply(1:ncol(rand_eff), function(xx) 
    rep(rand_eff[,xx], times = lvl1ss)))
  colnames(reff) <- c(unlist(lapply(1:ncol(rand_eff), function(xx) paste("b", xx-1, sep = ""))))
  
  Zmat <- model.matrix(random, data.frame(Xmat))

  sim_data <- data_glm_nested(Xmat, Zmat, fixed_param, rand_eff, n, p = lvl1ss)
  
  Xmat <- data.frame(Xmat,reff,sim_data)
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
#' @param fixed_param Fixed effect parameter values (i.e. beta weights).  Must be same length as fixed.
#' @param random_param A list of named elements that must contain: 
#'             random_var = variance of random parameters,
#'             rand_gen = Name of simulation function for random effects.
#'          Optional elements are:
#'             ther: Theorectial mean and variance from rand_gen,
#'             ther_sim: Simulate mean/variance for standardization purposes,
#'             cor_vars: Correlation between random effects,
#'             ...: Additional parameters needed for rand_gen function.
#' @param random_param3 A list of named elements that must contain: 
#'             random_var = variance of random parameters,
#'             rand_gen = Name of simulation function for random effects.
#'          Optional elements are:
#'             ther: Theorectial mean and variance from rand_gen,
#'             ther_sim: Simulate mean/variance for standardization purposes,
#'             cor_vars: Correlation between random effects,
#'             ...: Additional parameters needed for rand_gen function.
#' @param cov_param List of mean, sd (standard deviations), and var_type for fixed effects. 
#'  Does not include intercept, time, factors, or interactions. 
#'  var_type must be either "lvl1" or "lvl2". Must be same order as fixed formula above.
#' @param k Number of third level clusters.
#' @param n Cluster sample size.
#' @param p Within cluster sample size.
#' @param data_str Type of data. Must be "cross", "long", or "single".
#' @param cor_vars A vector of correlations between variables.
#' @param fact_vars A nested list of factor, categorical, or ordinal variable specification, 
#'      each list must include numlevels and var_type (must be "lvl1" or "lvl2");
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
#' fixed_param <- c(4, 2, 6, 2.3, 7, 0)
#' random_param <- list(random_var = c(7, 4, 2), rand_gen = 'rnorm')
#' random_param3 <- list(random_var = c(4, 2), rand_gen = 'rnorm')
#' cov_param <- list(mean = c(0, 0, 0), sd = c(1.5, 4, 2),
#' var_type = c("lvl1", "lvl2", "lvl3"))
#' k <- 10
#' n <- 150
#' p <- 30
#' data_str <- "long"
#' temp.three <- sim_glm_nested3(fixed, random, random3, fixed_param, random_param,
#' random_param3, cov_param, k, n, p, data_str = data_str)
#' head(temp.three)
#' }
#' @export 
sim_glm_nested3 <- function(fixed, random, random3, fixed_param, random_param = list(), 
                            random_param3 = list(), cov_param, k, n, p,
                            data_str, cor_vars = NULL, fact_vars = list(NULL),
                            unbal = FALSE, unbal3 = FALSE, unbalCont = NULL, unbalCont3 = NULL) {
  
  # if(randCor > 1 | randCor < -1 | randCor3 > 1 | randCor3 < -1) 
  #   stop("Random effect correlation out of range")
  
  fixed_vars <- attr(terms(fixed),"term.labels")    ##Extracting fixed effect term labels
  rand.vars <- attr(terms(random),"term.labels")   ##Extracting random effect term labels
  rand.vars3 <- attr(terms(random3),"term.labels")   ##Extracting random effect term labels
  
  if(length(rand.vars)+1 != length(random_param$random_var)) stop("Random lengths not equal")
  if(length(rand.vars3)+1 != length(random_param3$random_var)) stop("Third level random lengths not equal")
  if({length(fixed_vars)+1} != {length(fixed_param)}) stop("Fixed lengths not equal")
  
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
  
  rand_eff <- do.call(sim_rand_eff, c(random_param, n = n))
  rand_eff3 <- do.call(sim_rand_eff, c(random_param3, n = k))
  
  Xmat <- sim_fixef_nested3(fixed, fixed_vars, cov_param, k, n = lvl2ss, 
                            p = lvl1ss, data_str = data_str, cor_vars = cor_vars, 
                            fact_vars = fact_vars)
  
  reff <- do.call("cbind", lapply(1:ncol(rand_eff), function(xx) 
    rep(rand_eff[,xx], times = lvl1ss)))
  colnames(reff) <- c(unlist(lapply(1:ncol(rand_eff), function(xx) paste("b", xx-1, "_2", sep = ""))))
  
  reff3 <- do.call("cbind", lapply(1:ncol(rand_eff3), function(xx) 
    rep(rand_eff3[,xx], times = lvl2ss)))
  colnames(reff3) <- c(unlist(lapply(1:ncol(rand_eff3), function(xx) paste("b", xx-1, "_3", sep = ""))))
  
  Zmat <- model.matrix(random, data.frame(Xmat))
  Zmat3 <- model.matrix(random3, data.frame(Xmat))
  
  sim_data <- data_glm_nested3(Xmat, Zmat, Zmat3, fixed_param, rand_eff, rand_eff3,
                               k, n = lvl2ss, p = lvl1ss)
  
  Xmat <- data.frame(Xmat, reff, sim_data)
  Xmat$withinID <- unlist(lapply(1:length(lvl1ss), function(xx) 1:lvl1ss[xx]))
  Xmat$clustID <- rep(1:n, times = lvl1ss)
  Xmat$clust3ID <- rep(1:k, times = lvl3ss)
  return(Xmat)
}



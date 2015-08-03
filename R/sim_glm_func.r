#' Simulation logistic regression model
#' 
#' @param fixed One sided formula for fixed effects in the simulation.  To suppress intercept add -1 to formula.
#' @param fixed.param Fixed effect parameter values (i.e. beta weights).  Must be same length as fixed.
#' @param cov.param List of mean and sd (standard deviation) for fixed effects. Does not include intercept, time, or 
#'   interactions. Must be same order as fixed formula above.
#' @param n Cluster sample size.
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
sim_glm_single <- function(fixed, fixed.param, cov.param, n) {
  
  fixed.vars <- attr(terms(fixed),"term.labels")    ##Extracting fixed effect term labels
  
  if({length(fixed.vars)+1} != {length(fixed.param)}) stop("Fixed lengths not equal")
  
  Xmat <- sim_fixef_single(fixed, fixed.vars, n, cov.param, fact.vars)
  
  sim.data <- data_glm_single(Xmat, fixed.param, n)
  
  Xmat <- data.frame(Xmat,sim.data)
  Xmat$ID <- 1:n
  return(Xmat)
  
}

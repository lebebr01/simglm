#' Function to simulate continuous random effects.
#'
#' Input simulation parameters and returns random effects.
#'
#' Simulates continuous random effects for the master function \code{\link{sim_reg}} when
#' simulating a linear mixed model, both cross sectional and longitudinal. 
#' Allows the ability to simulate random effects from any generating distribution in R.
#'
#' @param random_var Variance of random effects. Must be same length as random.
#' @param n Cluster sample size.
#' @param rand_gen The generating function used (e.g. rnorm).
#' @param ther A vector of length two that specifies the theoretical mean and
#'              standard deviation of the rand_gen. This would commonly be used
#'              to standardize the generating variable to have a mean of 0 and
#'              standard deviation of 1 to meet model assumptions. The variable
#'              is then rescaled to have the variance specified by random_var.
#' @param ther_sim A TRUE/FALSE flag indicating whether the error simulation 
#'  function should be simulated, that is should the mean and standard deviation
#'  used for standardization be simulated.
#' @param cor_vars A vector of correlations between random effects. 
#' @param ... Additional values that need to be passed to the function
#'             called from rand_gen.
#' @export
sim_rand_eff_cont <- function(random_var, n, rand_gen, ther = c(0, 1),
                         ther_sim = FALSE, cor_vars = NULL, ...) {

  # Look to edit this with match.arg and switch functions
  if(ther_sim) {
    ther_val <- sapply(X = 1000000, FUN = rand_gen, ...)
    ther <- c(mean(ther_val), sd(ther_val))
  }
  
  reff <- do.call('cbind', lapply(seq_along(random_var), function(xx)  
    standardize(sapply(n, FUN = rand_gen, ...),
                mean = ther[1], sd = ther[2])))
  
  if(is.null(cor_vars)) {
    reff <- reff %*% chol(diag(length(random_var)) * random_var)
  } else {
    c_mat <- matrix(nrow = length(random_var), ncol = length(random_var))
    diag(c_mat) <- 1
    c_mat[upper.tri(c_mat)] <- c_mat[lower.tri(c_mat)] <- cor_vars
    cov <- diag(sqrt(random_var)) %*% 
      c_mat %*% diag(sqrt(random_var))
    es <- eigen(cov, symmetric = TRUE)
    ev <- es$values
    reff <- t(es$vectors %*% diag(sqrt(pmax(ev, 0)), length(random_var)) %*% 
                t(reff))
  }

 reff
}

#' Function to simulate categorical random effects.
#'
#' Input simulation parameters and returns random effects.
#'
#' Simulates categorical random effects for the master function \code{\link{sim_reg}} when
#' simulating a linear mixed model, both cross sectional and longitudinal. 
#' Allows the ability to simulate random effects from any generating distribution in R.
#' 
#' @param n Number of random effects to generate
#' @param numlevels Scalar indicating the number of levels for categorical, 
#'   factor, or discrete variable
#' @param replace Whether to replace levels of categorical variable, TRUE/FALSE
#' @param prob Probability of levels for variable, must be same length as 
#'  numlevels
#' @param var_type Variable type for the variable, must be either 
#'   "level1", "level2", "level3", or "single"
#' @param value_labels Optional argument with value labels for variable, 
#'        converts variable to factor.
#' @export
#' 
sim_rand_eff_cat <- function(n, numlevels, replace = TRUE, prob = NULL, 
                             value_labels = NULL) {
 
  cat_rand <- sample(x = numlevels, size = n, replace = replace, 
                     prob = prob)
  
  if(!is.null(value_labels)) {
    if(length(value_labels) != numlevels) { 
      stop("value_labels must be same length as numlevels") 
    }
    cat_rand <- factor(cat_rand, labels = value_labels)
  }
  
  cat_rand
}

#' Master function to simulate random effects
#' 
#' Input simulation parameters and returns random effects.
#'
#' Simulates categorical random effects for the master function \code{\link{sim_reg}} when
#' simulating a linear mixed model, both cross sectional and longitudinal. 
#' Allows the ability to simulate random effects from any generating distribution in R.
#' 
#' @param random One sided formula for random effects in the simulation. 
#' @param random_vars Character vector of covariates for design matrix.
#' @export
#' 
sim_rand_eff <- function(random, random_vars) {
  
  n.vars <- length(random_vars)
  n.int <- length(grep(":",random_vars))
  if(n.int > 0) {
    int.loc <- grep(":", random_vars)
  } else {
    int.loc <- 0
  }
  fact.loc <- grep("\\.f$|\\.o$|\\.c$|_f$|_c$|_o$", 
                   random_vars, ignore.case = TRUE)  
  n.fact <- length(fact.loc[fact.loc != int.loc])
  n.cont <- length(cov_param[[1]])
  
  if(length(fact.loc) > 0){
    random_vars <- c(random_vars[-c(fact.loc, int.loc)], random_vars[fact.loc], 
                    random_vars[int.loc])
  }
  
  if(n.fact > 0){
    if(!any(grepl("single", fact_vars$var_type))){
      stop("All variables must have var_type = 'single'")
    }
  }
  
}

#' Simulation single level logistic regression model
#' 
#' Takes simulation parameters as inputs and returns simulated data.
#' 
#' Simulates data for the simple logistic regression models.  Returns 
#' a data frame with ID variables, fixed effects, and many other variables
#' to help when running simulation studies.
#' 
#' @param fixed One sided formula for fixed effects in the simulation.  
#'   To suppress intercept add -1 to formula.
#' @param fixed_param Fixed effect parameter values (i.e. beta weights).  
#'   Must be same length as fixed.
#' @param cov_param List of arguments to pass to the continuous generating 
#'   function, must be the same order as the variables specified in fixed. 
#'   This list does not include intercept, time, factors, or 
#'   interactions. Required arguments include:
#'   \itemize{
#'     \item dist_fun: This is a quoted R distribution function.
#'     \item var_type: This is the level of variable to generate. Must be 
#'       'single'.
#'       Must be same order as fixed formula above.
#'   }
#'   Optional arguments to the distribution functions are in a nested list,
#'    see the examples or vignettes for example code.
#' @param n Cluster sample size.
#' @param data_str Type of data. Must be "cross", "long", or "single".
#' @param cor_vars A vector of correlations between variables.
#' @param fact_vars A nested list of factor, categorical, or ordinal variable 
#'      specification, each list must include:
#'   \itemize{
#'        \item numlevels = Number of levels for ordinal or factor variables.
#'        \item var_type = Must be 'single', 'lvl1', 'lvl2', or 'lvl3'.
#'    }
#'    Optional arguments include:
#'    \itemize{
#'        \item replace
#'        \item prob
#'        \item value.labels
#'    }
#'     See also \code{\link{sample}} for use of these optional arguments.
#' @param contrasts An optional list that specifies the contrasts to be used 
#'      for factor variables (i.e. those variables with .f or .c). 
#'      See \code{\link{contrasts}} for more detail.
#' @param outcome_type A vector specifying the type of outcome, must be either
#'   logistic or poisson. Logitstic outcome will be 0/1 and poisson outcome will
#'   be counts.
#' @param knot_args A nested list of named knot arguments. See \code{\link{sim_knot}} 
#'  for more details. Arguments must include:
#'    \itemize{
#'      \item var
#'      \item knot_locations
#'    }
#' @param ... Not currently used.
#' @examples 
#' # generating parameters for single level regression
#' set.seed(2)
#' fixed <- ~1 + act + diff + numCourse + act:numCourse
#' fixed_param <- c(0.1, -0.2, 0.15, 0.5, -0.02)
#' cov_param <- list(dist_fun = c('rnorm', 'rnorm', 'rnorm'),
#'    var_type = c("single", "single", "single"),
#'    opts = list(list(mean = 0, sd = 4),
#'    list(mean = 0, sd = 3),
#'    list(mean = 0, sd = 3)))
#' n <- 150
#' temp_single <- sim_glm(fixed = fixed, fixed_param = fixed_param, 
#'   cov_param = cov_param, n = n, data_str = "single", 
#'   outcome_type = 'logistic')
#'   
#' @export
sim_glm_single <- function(fixed, fixed_param, cov_param, n, 
                           data_str, cor_vars = NULL, fact_vars = list(NULL),
                           contrasts = NULL, outcome_type, 
                           knot_args = list(NULL), ...) {
  
  fixed_vars <- attr(terms(fixed),"term.labels")    
  
  Xmat <- sim_fixef_single(fixed, fixed_vars, n, cov_param, cor_vars, 
                           fact_vars, contrasts, knot_args = knot_args)
  
  if(ncol(Xmat) != length(fixed_param)) {
    stop(paste(length(fixed_param), 'parameters specified for', ncol(Xmat), 
               'variables in design matrix'))
  }
  
  sim_data <- data_glm_single(Xmat, fixed_param, n, outcome_type)
  
  Xmat <- data.frame(Xmat,sim_data)
  Xmat$ID <- 1:n
  
  Xmat <- Xmat[, !duplicated(colnames(Xmat))]
  
  Xmat
}

#' Simulate two level logistic regression model
#' 
#' Takes simulation parameters as inputs and returns simulated data.
#' 
#' Simulates data for the nested logistic regression models.  Returns a 
#' data frame with ID variables, fixed effects, random effects, and many
#' other variables to help when running simulation studies.
#' 
#' @param fixed One sided formula for fixed effects in the simulation.  
#'    To suppress intercept add -1 to formula.
#' @param random One sided formula for random effects in the simulation. 
#'    Must be a subset of fixed.
#' @param fixed_param Fixed effect parameter values (i.e. beta weights). 
#'    Must be same length as fixed.
#' @param random_param A list of named elements that must contain: 
#'   \itemize{
#'      \item  random_var = variance of random parameters,
#'      \item  rand_gen = Name of simulation function for random effects.
#'   }
#'          Optional elements are:
#'   \itemize{
#'      \item ther: Theorectial mean and variance from rand_gen,
#'      \item ther_sim: Simulate mean/variance for standardization purposes,
#'      \item cor_vars: Correlation between random effects,
#'      \item ...: Additional parameters needed for rand_gen function.
#'    }
#' @param cov_param List of arguments to pass to the continuous generating 
#'   function, must be the same order as the variables specified in fixed. 
#'   This list does not include intercept, time, factors, or 
#'   interactions. Required arguments include:
#'   \itemize{
#'     \item dist_fun: This is a quoted R distribution function.
#'     \item var_type: This is the level of variable to generate. Must be 
#'       'level1' or 'level2'.
#'       Must be same order as fixed formula above.
#'   }
#'   Optional arguments to the distribution functions are in a nested list,
#'    see the examples or vignettes for example code.
#' @param n Cluster sample size.
#' @param p Within cluster sample size.
#' @param data_str Type of data. Must be "cross", "long", or "single".
#' @param cor_vars A vector of correlations between variables.
#' @param fact_vars A nested list of factor, categorical, or ordinal variable 
#'      specification, each list must include:
#'   \itemize{
#'        \item numlevels = Number of levels for ordinal or factor variables.
#'        \item var_type = Must be 'level1' or 'level2'.
#'    }
#'    Optional arguments include:
#'    \itemize{
#'        \item replace
#'        \item prob
#'        \item value.labels
#'    }
#'     See also \code{\link{sample}} for use of these optional arguments.
#' @param unbal A vector of sample sizes for the number of observations for 
#'  each level 2 cluster. Must have same length as level two sample size n. 
#'  Alternative specification can be TRUE, which uses additional argument, 
#'  unbal_design.
#' @param unbal_design When unbal = TRUE, this specifies the design for unbalanced
#'  simulation in one of two ways. It can represent the minimum and maximum 
#'  sample size within a cluster via a named list. This will be drawn from a 
#'  random uniform distribution with min and max specified. 
#'  Secondly, the sample sizes within each cluster can be specified. 
#'  This takes the form of a vector that must have the same length 
#'  as the level two sample size.
#' @param contrasts An optional list that specifies the contrasts to be used 
#'   for factor variables (i.e. those variables with .f or .c). 
#'   See \code{\link{contrasts}} for more detail.
#' @param outcome_type A vector specifying the type of outcome, must be either
#'   logistic or poisson. Logitstic outcome will be 0/1 and poisson outcome will
#'   be counts.
#' @param cross_class_params A list of named parameters when cross classified 
#'  data structures are desired. Must include the following arguments:
#'   \itemize{
#'    \item num_ids: The number of cross classified clusters. These are in 
#'         addition to the typical cluster ids
#'    \item random_param: This argument is a list of arguments passed to 
#'       \code{\link{sim_rand_eff}}. These must include:
#'      \itemize{
#'       \item random_var: The variance of the cross classified random effect
#'       \item rand_gen: The random generating function used to generate the 
#'          cross classified random effect.
#'      }
#'      Optional elements are:
#'    \itemize{
#'        \item ther: Theorectial mean and variance from rand_gen,
#'        \item ther_sim: Simulate mean/variance for standardization purposes,
#'        \item cor_vars: Correlation between random effects,
#'        \item ...: Additional parameters needed for rand_gen function.
#'    } 
#'   }
#' @param knot_args A nested list of named knot arguments. See \code{\link{sim_knot}} 
#'  for more details. Arguments must include:
#'    \itemize{
#'      \item var
#'      \item knot_locations
#'    }
#' @param ... Not currently used.
#'      
#' @examples
#' # Longitudinal linear mixed model example
#' fixed <- ~1 + time + diff + act + time:act
#' random <- ~1 + time + diff
#' fixed_param <- c(0.1, -0.2, 0.15, 0.5, -0.02)
#' random_param <- list(random_var = c(7, 4, 2), rand_gen = 'rnorm')
#' cov_param <- list(dist_fun = c('rnorm', 'rnorm'),
#'    var_type = c("level1", "level2"),
#'    opts = list(list(mean = 0, sd = 1.5),
#'    list(mean = 0, sd = 4)))
#' n <- 150
#' p <- 30
#' data_str <- "long"
#' temp_long <- sim_glm(fixed, random, random3 = NULL, fixed_param, 
#' random_param, random_param3 = NULL,
#'  cov_param, k = NULL, n, p, data_str = data_str, outcome_type = 'logistic')
#' @export
sim_glm_nested <- function(fixed, random, fixed_param, random_param = list(), 
                           cov_param, n, p, data_str, cor_vars = NULL, 
                           fact_vars = list(NULL), unbal = FALSE, 
                           unbal_design = NULL, contrasts = NULL, 
                           outcome_type, cross_class_params = NULL, 
                           knot_args = list(NULL), ...) {
  
  fixed_vars <- attr(terms(fixed),"term.labels")    
  rand.vars <- attr(terms(random),"term.labels")   
  
  if(length(rand.vars)+1 != length(random_param$random_var)) 
    stop("Random lengths not equal")

  if(unbal$level2 == FALSE) {
    lvl1ss <- rep(p, n)
    if(is.null(lvl1ss)) stop("lvl1ss is NULL")
  } else {
    if(length(unbal_design$level2) < 2) stop("Must specify unbal_design when unbal = TRUE")
    if(is.null(names(unbal_design$level2))) {
      if(length(unbal_design$level2) != n) stop('unbal_design must be same length as n')
      lvl1ss <- unbal_design$level2
    } else {
      lvl1ss <- round(runif(n = n, min = unbal_design$level2$min, 
                            max = unbal_design$level2$max), 0)
    }
  }
  
  Xmat <- sim_fixef_nested(fixed, fixed_vars, cov_param, n, lvl1ss, 
                           data_str = data_str, cor_vars = cor_vars, 
                           fact_vars = fact_vars, contrasts = contrasts,
                           knot_args = knot_args)
  
  rand_eff <- do.call(sim_rand_eff, c(random_param, n = n))
  
  reff <- do.call("cbind", lapply(1:ncol(rand_eff), function(xx) 
    rep(rand_eff[,xx], times = lvl1ss)))
  colnames(reff) <- c(unlist(lapply(1:ncol(rand_eff), function(xx) 
    paste("b", xx-1, sep = ""))))
  
  if(any(grepl("\\.f$|\\.c$|_f$|_c$|\\.k$|_k$", fixed_vars, ignore.case = TRUE))) {
    if(ncol(Xmat$Xmat) != length(fixed_param)) {
      stop(paste(length(fixed_param), 'parameters specified for', ncol(Xmat), 
                 'variables in design matrix'))
    }
    
    Zmat <- model.matrix(random, data.frame(Xmat$Xmat))
    
    sim_data <- data_glm_nested(Xmat$Xmat, Zmat, fixed_param, rand_eff, n, 
                                p = lvl1ss, outcome_type = outcome_type)
    
    Xmat <- dplyr::bind_cols(data.frame(Xmat$Xmat), 
                             data.frame(Xmat$Omat), 
                             data.frame(reff),
                             data.frame(sim_data))
  } else {
    if(ncol(Xmat) != length(fixed_param)) {
      stop(paste(length(fixed_param), 'parameters specified for', ncol(Xmat), 
                 'variables in design matrix'))
    }
    
    Zmat <- model.matrix(random, data.frame(Xmat))
    
    sim_data <- data_glm_nested(Xmat, Zmat, fixed_param, rand_eff, n, 
                                p = lvl1ss, outcome_type = outcome_type)
    
    Xmat <- data.frame(Xmat, reff, sim_data)
  }
  
  Xmat$withinID <- unlist(lapply(1:length(lvl1ss), function(xx) 1:lvl1ss[xx]))
  Xmat$clustID <- rep(1:n, times = lvl1ss)
  
  if(!is.null(cross_class_params)) {
    
    cross_eff <- cross_class(cross_class_params$num_ids, sum(lvl1ss), 
                             cross_class_params$random_param)
    
    Xmat <- dplyr::bind_cols(Xmat, cross_eff)
    Xmat$sim_data <- Xmat$sim_data + Xmat$c1
  }
  
  Xmat <- Xmat[, !duplicated(colnames(Xmat))]
  
  Xmat
}

#' Function to simulate three level nested data
#' 
#' Takes simulation parameters as inputs and returns simulated data.
#' 
#' Simulates data for the linear mixed model, both cross sectional and 
#' longitudinal data. Returns a data frame with ID variables, fixed effects, 
#' and many other variables useful to help when running simulation studies.
#' 
#' @seealso \code{\link{sim_reg}} for a convenient wrapper for all data 
#'   conditions.
#' 
#' @param fixed One sided formula for fixed effects in the simulation. 
#'   To suppress intercept add -1 to formula.
#' @param random One sided formula for random effects in the simulation. 
#'   Must be a subset of fixed.
#' @param random3 One sided formula for random effects at third level in the 
#'  simulation. Must be a subset of fixed (and likely of random).
#' @param fixed_param Fixed effect parameter values (i.e. beta weights). 
#'  Must be same length as fixed.
#' @param random_param A list of named elements that must contain: 
#'   \itemize{
#'      \item  random_var = variance of random parameters,
#'      \item  rand_gen = Name of simulation function for random effects.
#'   }
#'          Optional elements are:
#'   \itemize{
#'      \item ther: Theorectial mean and variance from rand_gen,
#'      \item ther_sim: Simulate mean/variance for standardization purposes,
#'      \item cor_vars: Correlation between random effects,
#'      \item ...: Additional parameters needed for rand_gen function.
#'    }
#' @param random_param3 A list of named elements that must contain: 
#'    \itemize{
#'        \item random_var = variance of random parameters,
#'        \item rand_gen = Name of simulation function for random effects.
#'    }
#'          Optional elements are:
#'    \itemize{
#'        \item ther: Theorectial mean and variance from rand_gen,
#'        \item ther_sim: Simulate mean/variance for standardization purposes,
#'        \item cor_vars: Correlation between random effects,
#'        \item ...: Additional parameters needed for rand_gen function.
#'    }
#' @param cov_param List of arguments to pass to the continuous generating 
#'   function, must be the same order as the variables specified in fixed. 
#'   This list does not include intercept, time, factors, or 
#'   interactions. Required arguments include:
#'   \itemize{
#'     \item dist_fun: This is a quoted R distribution function.
#'     \item var_type: This is the level of variable to generate. Must be 
#'       'level1', 'level2', or 'level3'. 
#'       Must be same order as fixed formula above.
#'   }
#'   Optional arguments to the distribution functions are in a nested list,
#'    see the examples or vignettes for example code.
#' @param k Number of third level clusters.
#' @param n Level two sample size within each level three cluster.
#' @param p Within cluster sample size within each level two cluster.
#' @param data_str Type of data. Must be "cross", "long", or "single".
#' @param cor_vars A vector of correlations between variables.
#' @param fact_vars A nested list of factor, categorical, or ordinal variable 
#'      specification, each list must include:
#'   \itemize{
#'        \item numlevels = Number of levels for ordinal or factor variables.
#'        \item var_type = Must be 'single', 'level1', 'level2', or 'level3'.
#'    }
#'    Optional arguments include:
#'    \itemize{
#'        \item replace
#'        \item prob
#'        \item value.labels
#'    }
#'     See also \code{\link{sample}} for use of these optional arguments.
#' @param unbal A named TRUE/FALSE list specifying whether unbalanced simulation 
#'  design is desired. The named elements must be: "level2" or "level3" representing
#'  unbalanced simulation for level two and three respectively. Default is FALSE,
#'  indicating balanced sample sizes at both levels.
#' @param unbal_design When unbal = TRUE, this specifies the design for unbalanced
#'  simulation in one of two ways. It can represent the minimum and maximum 
#'  sample size within a cluster via a named list. This will be drawn from a 
#'  random uniform distribution with min and max specified. 
#'  Secondly, the actual sample sizes within each cluster
#'  can be specified. This takes the form of a vector that must have the same length 
#'  as the level two or three sample size. These are specified as a named list in which
#'  level two sample size is controlled via "level2" and level three sample size is 
#'  controlled via "level3".
#' @param contrasts An optional list that specifies the contrasts to be used 
#'  for factor variables (i.e. those variables with .f or .c). 
#'  See \code{\link{contrasts}} for more detail.
#' @param outcome_type A vector specifying the type of outcome, must be either
#'   logistic or poisson. Logitstic outcome will be 0/1 and poisson outcome will
#'   be counts.
#' @param cross_class_params A list of named parameters when cross classified 
#'  data structures are desired. Must include the following arguments:
#'   \itemize{
#'    \item num_ids: The number of cross classified clusters. These are in 
#'         addition to the typical cluster ids
#'    \item random_param: This argument is a list of arguments passed to 
#'       \code{\link{sim_rand_eff}}. These must include:
#'      \itemize{
#'       \item random_var: The variance of the cross classified random effect
#'       \item rand_gen: The random generating function used to generate the 
#'          cross classified random effect.
#'      }
#'      Optional elements are:
#'    \itemize{
#'        \item ther: Theorectial mean and variance from rand_gen,
#'        \item ther_sim: Simulate mean/variance for standardization purposes,
#'        \item cor_vars: Correlation between random effects,
#'        \item ...: Additional parameters needed for rand_gen function.
#'    } 
#'   }
#' @param knot_args A nested list of named knot arguments. See \code{\link{sim_knot}} 
#'  for more details. Arguments must include:
#'    \itemize{
#'      \item var
#'      \item knot_locations
#'    }
#' @param ... Not currently used.
#' 
#' @examples 
#' # Three level example
#' fixed <- ~1 + time + diff + act + actClust + time:act
#' random <- ~1 + time + diff
#' random3 <- ~ 1 + time
#' fixed_param <- c(0.1, -0.2, 0.15, 0.5, -0.02, 0.04)
#' random_param <- list(random_var = c(7, 4, 2), rand_gen = 'rnorm')
#' random_param3 <- list(random_var = c(4, 2), rand_gen = 'rnorm')
#' cov_param <- list(dist_fun = c('rnorm', 'rnorm', 'rnorm'), 
#'    var_type = c("level1", "level2", "level3"),
#'    opts = list(list(mean = 0, sd = 1.5),
#'    list(mean = 0, sd = 4),
#'    list(mean = 0, sd = 2)))
#' k <- 10
#' n <- 15
#' p <- 10
#' data_str <- "long"
#' temp_three <- sim_glm(fixed, random, random3, fixed_param, random_param, 
#'   random_param3, cov_param, k,n, p, data_str = data_str, 
#'   outcome_type = 'logistic')
#'   
#' @export 
sim_glm_nested3 <- function(fixed, random, random3, fixed_param, 
                            random_param = list(), random_param3 = list(), 
                            cov_param, k, n, p, data_str, cor_vars = NULL, 
                            fact_vars = list(NULL), 
                            unbal = list("level2" = FALSE, "level3" = FALSE), 
                            unbal_design = list("level2" = NULL, "level3" = NULL),
                            contrasts = NULL, 
                            outcome_type, cross_class_params = NULL, 
                            knot_args = list(NULL), ...) {

  fixed_vars <- attr(terms(fixed),"term.labels")    
  rand.vars <- attr(terms(random),"term.labels")   
  rand.vars3 <- attr(terms(random3),"term.labels")  
  
  if(length(rand.vars)+1 != length(random_param$random_var)) 
    stop("Random lengths not equal")
  if(length(rand.vars3)+1 != length(random_param3$random_var)) 
    stop("Third level random lengths not equal")
  
  if(unbal$level3 == FALSE) {
    lvl2ss <- rep(n, k)
    n <- sum(lvl2ss)
  } else {
    if(is.null(unbal_design$level3)) {
      stop("Must specify unbal_design$level3 when unbal$level3 = TRUE")
    }
    if(is.null(names(unbal_design$level3))) {
      if(length(unbal_design$level3) != k) {
        stop('unbal_design$level3 must be same length as k')
      }
      lvl2ss <- unbal_design$level3
    } else {
      lvl2ss <- round(runif(n = k, min = unbal_design$level3$min, 
                            max = unbal_design$level3$max), 0)
    }
    n <- sum(lvl2ss)
  }
  
  if(unbal$level2 == FALSE) {
    lvl1ss <- rep(p, n)
    if(is.null(lvl1ss)) stop("lvl1ss is NULL")
  } else {
    if(is.null(unbal_design$level2)) {
      stop("Must specify unbal_design$level2 when unbal$level2 = TRUE")
    }
    if(is.null(names(unbal_design$level2))) {
      if(length(unbal_design$level2) != n) {
        stop('unbal_design$level2 must be same length as n')
      }
      lvl1ss <- unbal_design$level2
    } else {
      lvl1ss <- round(runif(n = n, min = unbal_design$level2$min, 
                            max = unbal_design$level2$max), 0)
    }
  }
  
  end <- cumsum(lvl2ss)
  beg <- c(1, cumsum(lvl2ss) + 1)
  beg <- beg[-length(beg)]
  
  lvl3ss <- sapply(lapply(1:length(beg), function(xx) 
    lvl1ss[beg[xx]:end[xx]]), sum)
  
  Xmat <- sim_fixef_nested3(fixed, fixed_vars, cov_param, k, n = lvl2ss, 
                            p = lvl1ss, data_str = data_str, 
                            cor_vars = cor_vars, 
                            fact_vars = fact_vars, contrasts = contrasts,
                            knot_args = knot_args)
  
  rand_eff <- do.call(sim_rand_eff, c(random_param, n = n))
  rand_eff3 <- do.call(sim_rand_eff, c(random_param3, n = k))
  
  reff <- do.call("cbind", lapply(1:ncol(rand_eff), function(xx) 
    rep(rand_eff[,xx], times = lvl1ss)))
  colnames(reff) <- c(unlist(lapply(1:ncol(rand_eff), function(xx) 
    paste("b", xx-1, "_2", sep = ""))))
  
  reff3 <- do.call("cbind", lapply(1:ncol(rand_eff3), function(xx) 
    rep(rand_eff3[,xx], times = lvl3ss)))
  colnames(reff3) <- c(unlist(lapply(1:ncol(rand_eff3), function(xx) 
    paste("b", xx-1, "_3", sep = ""))))
  
  if(any(grepl("\\.f$|\\.c$|_f$|_c$|\\.k$|_k$", fixed_vars, ignore.case = TRUE))) {
    if(ncol(Xmat$Xmat) != length(fixed_param)) {
      stop(paste(length(fixed_param), 'parameters specified for', ncol(Xmat), 
                 'variables in design matrix'))
    }
    
    Zmat <- model.matrix(random, data.frame(Xmat$Xmat))
    Zmat3 <- model.matrix(random3, data.frame(Xmat$Xmat))
    
    sim_data <- data_glm_nested3(Xmat$Xmat, Zmat, Zmat3, fixed_param, rand_eff, 
                                 rand_eff3, k, n = lvl2ss, p = lvl1ss, 
                                 outcome_type = outcome_type)
    
    Xmat <- dplyr::bind_cols(data.frame(Xmat$Xmat), 
                             data.frame(Xmat$Omat), 
                             data.frame(reff),
                             data.frame(reff3),
                             data.frame(sim_data))
  } else {
    if(ncol(Xmat) != length(fixed_param)) {
      stop(paste(length(fixed_param), 'parameters specified for', ncol(Xmat), 
                 'variables in design matrix'))
    }
    Zmat <- model.matrix(random, data.frame(Xmat))
    Zmat3 <- model.matrix(random3, data.frame(Xmat))
    
    sim_data <- data_glm_nested3(Xmat, Zmat, Zmat3, fixed_param, rand_eff, 
                                 rand_eff3, k, n = lvl2ss, p = lvl1ss,
                                 outcome_type = outcome_type)
    
    Xmat <- data.frame(Xmat, reff, reff3, sim_data)
  }
  
  Xmat$withinID <- unlist(lapply(1:length(lvl1ss), function(xx) 1:lvl1ss[xx]))
  Xmat$clustID <- rep(1:n, times = lvl1ss)
  Xmat$clust3ID <- rep(1:k, times = lvl3ss)
  
  if(!is.null(cross_class_params)) {
    
    cross_eff <- cross_class(cross_class_params$num_ids, sum(lvl1ss), 
                             cross_class_params$random_param)
    
    Xmat <- dplyr::bind_cols(Xmat, cross_eff)
    Xmat$sim_data <- Xmat$sim_data + Xmat$c1
  }
  
  Xmat <- Xmat[, !duplicated(colnames(Xmat))]
  
  Xmat
}

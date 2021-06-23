#' Simulate categorical, factor, or discrete variables
#' 
#' Function that simulates discrete, factor, or categorical variables.  
#' Is essentially a wrapper around the sample function from base R.
#' 
#' @param k Number of third level clusters.
#' @param n Number of clusters or number of observations for single level
#' @param p Number of within cluster observations for multilevel
#' @param numlevels Scalar indicating the number of levels for categorical, 
#'   factor, or discrete variable
#' @param var_type Variable type for the variable, must be either 
#'   "level1", "level2", "level3", or "single"
#' @param ... Additional parameters passed to the sample function.
#' @export 
sim_factor <- function(k = NULL, n, p, numlevels, 
                       var_type = c('level1', 'level2', 'level3', 'single'),
                       ...) {
  end <- cumsum(n)
  beg <- c(1, cumsum(n) + 1)
  beg <- beg[-length(beg)]
  
  if(!is.null(k)) {
    lvl3ss <- sapply(lapply(seq_along(beg), function(xx) 		
      p[beg[xx]:end[xx]]), sum)
  }
  
  var_type <- match.arg(var_type)
  
  cat_var <- switch(var_type,
                    single = base::sample(x = numlevels, size = n, ...),
                    level3 = rep(base::sample(x = numlevels, size = k, ...), times = lvl3ss),
                    level2 = rep(base::sample(x = numlevels, size = length(p), ...), times = p),
                    level1 = base::sample(x = numlevels, size = sum(p), ...)
  )
  cat_var
}


#' Simulate categorical, factor, or discrete variables
#' 
#' Function that simulates discrete, factor, or categorical variables.  
#' Is essentially a wrapper around the sample function from base R.
#' 
#' @param n A list of sample sizes.
#' @param levels Scalar indicating the number of levels for categorical, 
#'   factor, or discrete variable. Can also specify levels as a character vector.
#' @param var_level The level the variable should be simulated at. This can either 
#'      be 1, 2, or 3 specifying a level 1, level 2, or level 3 variable 
#'      respectively.
#' @param replace TRUE/FALSE indicating whether levels should be sampled with 
#'   replacement. Default is TRUE.
#' @param ... Additional parameters passed to the sample function.
#' @export 
sim_factor2 <- function(n, levels, var_level = 1, replace = TRUE,
                       ...) {
  if(var_level == 1) {
    cat_var <- base::sample(x = levels, size = n[['level1']], 
                            replace = replace, ...)
  } else {
    if(var_level == 2) {
      cat_var <- rep(base::sample(x = levels, size = sum(n[['level2']]), 
                                  replace = replace, ...),
                      times = n[['level1']])
    } else {
      cat_var <- rep(base::sample(x = levels, size = n[['level3']], 
                                  replace = replace, ...),
                      times = n[['level3_total']])
    }
  }
  
  if(is.character(levels)) {
    cat_var <- factor(cat_var, levels = levels, labels = levels)
  }
  
  cat_var
}


#' Simulate continuous variables
#' 
#' Function that simulates continuous variables. Any distribution function in 
#' R is supported.
#' 
#' @param k Number of third level clusters.
#' @param n Number of clusters or number of observations for single level
#' @param p Number of within cluster observations for multilevel
#' @param dist_fun A distribution function. This argument takes a quoted
#'      R distribution function (e.g. 'rnorm').
#' @param var_type Variable type for the variable, must be either "level1", 
#'      "level2", "level3", or "single"
#' @param ... Additional parameters to pass to the dist_fun argument.
#' @export 
sim_continuous <- function(k = NULL, n, p, dist_fun,
                           var_type = c('level1', 'level2', 'level3', 'single'),
                           ...) {
  
  end <- cumsum(n)
  beg <- c(1, cumsum(n) + 1)
  beg <- beg[-length(beg)]
  
  if(!is.null(k)) {
    lvl3ss <- sapply(lapply(seq_along(beg), function(xx) 		
      p[beg[xx]:end[xx]]), sum)
  }
  
  var_type <- match.arg(var_type)
  
  contVar <- switch(var_type,
                    single = unlist(lapply(n, FUN = dist_fun, ...)),
                    level3 = rep(unlist(lapply(k, FUN = dist_fun, ...)), 
                                 times = lvl3ss),
                    level2 = rep(unlist(lapply(length(p), FUN = dist_fun, ...)), 
                                 times = p),
                    level1 = unlist(lapply(sum(p), FUN = dist_fun, ...))
  )
  contVar
}

#' Simulate continuous variables
#' 
#' Function that simulates continuous variables. Any distribution function in 
#' R is supported.
#' 
#' @param n A list of sample sizes.
#' @param dist A distribution function. This argument takes a quoted
#'      R distribution function (e.g. 'rnorm'). Default is 'rnorm'.
#' @param var_level The level the variable should be simulated at. This can either 
#'      be 1, 2, or 3 specifying a level 1, level 2, or level 3 variable 
#'      respectively.
#' @param variance The variance for random effect simulation.
#' @param ther_sim A TRUE/FALSE flag indicating whether the error simulation 
#'  function should be simulated, that is should the mean and standard deviation
#'  used for standardization be simulated. 
#' @param ther_val A vector of 2 that should include the theoretical mean and 
#'  standard deviation of the generating function.
#' @param ... Additional parameters to pass to the dist_fun argument.
#' @export 
sim_continuous2 <- function(n, dist = 'rnorm', var_level = 1, 
                            variance = NULL, ther_sim = FALSE, ther_val = NULL, 
                            ...) {
  
  if(var_level == 1) {
    cont_var <- unlist(lapply(n[['level1']], FUN = dist, ...))
  } else {
    if(var_level == 2) {
      cont_var <- rep(unlist(lapply(sum(n[['level2']]), FUN = dist, ...)),
                      times = n[['level1']])
    } else {
      cont_var <- rep(unlist(lapply(n[['level3']], FUN = dist, ...)),
                      times = n[['level3_total']])
    }
  }
  
  if(!is.null(variance)) {
    if(ther_sim) {
      ther_val <- do.call(dist, c(list(n = 10000000), ...))
      ther <- c(mean(ther_val), sd(ther_val))
      
      cont_var <- standardize(cont_var, ther[1], ther[2])
    }
    if(!is.null(ther_val)) {
      cont_var <- standardize(cont_var, ther_val[1], ther_val[2])
    }
      cont_var <- cont_var %*% chol(c(variance))
  }
  cont_var
}

#' Simulate knot locations
#' 
#' Function that generates knot locations. An example of usefulness of this funciton
#' would be with generation of interrupted time series data. Another application may
#' be with simulation of piecewise linear data structures.
#' 
#' @param var Variable used to create knots in the data. 
#' @param knot_locations The locations to create knots. These need to be specified 
#'   with the scale of the variable in mind. See examples.
#' @param right logical, indicating if the intervals should be closed on the right
#'   (and open on the left) or vice versa. See \code{\link{cut}} for more details. 
#'   Defaults to FALSE, which is likely most desirable behavior in this context.
#' @export 
#' @examples
#' sim_knot(0:10, knot_locations = c(4, 9))
#' sim_knot(rnorm(100), knot_locations = c(-1, 1.5))
#' sim_knot(0:8, knot_locations = 5)   
#' sim_knot(0:8, knot_locations = 5, right = TRUE)  
sim_knot <- function(var, knot_locations, right = FALSE) {
  
  if(!right) {
    knot_locat <- c(min(var), knot_locations, (max(var) + 1))
  } else {
    knot_locat <- c((min(var) - 1), knot_locations, (max(var)))
  }
  
  cut(var, knot_locat, labels = FALSE, right = right) - 1
}

#' Simulate knot locations
#' 
#' Function that generates knot locations. An example of usefulness of this funciton
#' would be with generation of interrupted time series data. Another application may
#' be with simulation of piecewise linear data structures.
#' 
#' @param data Data to pass to the sim_knot2 function to determine knot 
#'   location.
#' @param sim_args A named list with special model formula syntax. See details and examples
#'   for more information. The named list may contain the following:
#'   \itemize{
#'     \item fixed: This is the fixed portion of the model (i.e. covariates)
#'     \item random: This is the random portion of the model (i.e. random effects)
#'     \item error: This is the error (i.e. residual term).
#'   }
#' @param data Mostly internal argument.
#' @export
simulate_knot <- function(data, sim_args) {
  
  purrr::invoke_map("sim_knot2", 
                    sim_args[['knot']], 
                    data = data
  ) %>% data.frame()
  
}

sim_knot2 <- function(data, variable, knot_locations, right = FALSE, ...) {
  
  if(!right) {
    knot_locat <- c(min(data[[variable]]), knot_locations, (max(data[[variable]]) + 1))
  } else {
    knot_locat <- c((min(data[[variable]]) - 1), knot_locations, (max(data[[variable]])))
  }
  
  cut(data[[variable]], knot_locat, labels = FALSE, right = right, ...) - 1
  
}

#' Simulate Time
#' 
#' This function simulates data for the time variable of longitudinal data.
#' 
#' @param n Sample size of the levels.
#' @param time_levels The values the time variable should take. If NULL (default),
#'   the time values are discrete integers starting at 0 and going to n - 1.
#' @param ... Currently not used.
#' 
#' @export 
sim_time <- function(n, time_levels = NULL, ...) {
  
  if(is.null(time_levels)) {
    do.call('c', 
            lapply(seq_along(n[['level1']]), function(xx) 
              0:(n[['level1']][xx] - 1)))
  } else {
    do.call('c', 
            lapply(seq_along(n[['level1']]), function(xx)
              time_levels[1:n[['level1']][xx]]))
  }
  
}

sim_variable <- function(var_type = c("continuous", "factor", "ordinal", 
                                      'time'), ...) {
  var_type <- match.arg(var_type)
  
  switch(var_type,
    continuous = sim_continuous2(...),
    factor = sim_factor2(...),
    ordinal = sim_factor2(...),
    #knot = sim_knot2(...),
    time = sim_time(...)
  )
}

#' Tidy fixed effect formula simulation
#' 
#' This function simulates the fixed portion of the model using a formula syntax.
#' 
#' @param data Data simulated from other functions to pass to this function. Can pass
#'  NULL if first in simulation string.
#' @param sim_args A named list with special model formula syntax. See details and examples
#'   for more information. The named list may contain the following:
#'   \itemize{
#'     \item fixed: This is the fixed portion of the model (i.e. covariates)
#'     \item random: This is the random portion of the model (i.e. random effects)
#'     \item error: This is the error (i.e. residual term).
#'   }
#' @param ... Other arguments to pass to error simulation functions.
#' @importFrom purrr modify_if
#' 
#' @export 
simulate_fixed <- function(data, sim_args, ...) {
  
  fixed_formula <- parse_formula(sim_args)[['fixed']]
  
  fixed_vars <- attr(terms(fixed_formula), "term.labels")  
  
  if(any(grepl('^factor\\(', fixed_vars))) {
    fixed_vars <- gsub("factor\\(|\\)$", "", fixed_vars)
  }
  if(any(grepl('^ns\\(', fixed_vars))) {
    fixed_vars <- gsub("ns\\(|bs\\(|\\,.+\\)$", "", fixed_vars)
  }
  if(any(grepl("^poly\\(", fixed_vars))) {
    fixed_vars <- gsub("poly\\(|\\,.+\\)", "", fixed_vars)
  }
  
  if(is.null(data)) {
    n <- sample_sizes(sim_args[['sample_size']])
    ids <- create_ids(n, 
                      c('level1_id', parse_randomeffect(parse_formula(sim_args)[['randomeffect']])[['cluster_id_vars']]))
    Xmat <- purrr::invoke_map("sim_variable", 
                              sim_args[['fixed']],
                              n = n
    ) %>% 
      data.frame()
  } else {
    n <- compute_samplesize(data, sim_args)
    Xmat <- purrr::invoke_map("sim_variable", 
                              sim_args[['fixed']],
                              n = n
    ) %>% 
      data.frame()
  }
  

  
  if(!is.null(sim_args[['knot']])) {
    
    knot_names <- names(sim_args[['knot']])
    knot_loc <- grep(knot_names, fixed_vars)
    
    fixed_vars_knot <- fixed_vars[-knot_loc]
      
    if(any(grepl(":|^I", fixed_vars_knot))) {
      int_loc <- grep(":|^I", fixed_vars_knot)
      colnames(Xmat) <- fixed_vars_knot[-int_loc]
    } else {
      colnames(Xmat) <- fixed_vars_knot
    } 
    
    Xmat <- cbind(Xmat, 
                  simulate_knot(data = Xmat, sim_args = sim_args))
  }
  
  if(any(grepl(":|^I", fixed_vars))) {
    int_loc <- grep(":|^I", fixed_vars)
    colnames(Xmat) <- fixed_vars[-int_loc]
  } else {
    colnames(Xmat) <- fixed_vars
  } 

  if(any(unlist(lapply(seq_along(sim_args[['fixed']]), function(xx) 
    sim_args[['fixed']][[xx]]$var_type)) == 'factor') | 
    any(grepl("^ns|^poly", attr(terms(fixed_formula), "term.labels")))) {
    fixed_vars <- poly_ns_names(sim_args)
    
    if(any(grepl('^factor\\(', fixed_vars))) {
      fixed_vars <- gsub("factor\\(|\\)$", "", fixed_vars)
    }
    
    num_levels <- lapply(seq_along(sim_args[['fixed']]), function(xx) 
      sim_args[['fixed']][[xx]][['levels']])
    num_levels <- purrr::modify_if(num_levels, is.character, length)
    
    if(any(unlist(lapply(seq_along(sim_args[['fixed']]), function(xx) 
      num_levels[[xx]] > 2 & 
      sim_args[['fixed']][[xx]][['var_type']] == 'factor'))
      )) {
      fixed_vars <- factor_names(sim_args, fixed_vars)
    }
    
    Omat <- Xmat
    Xmat <- data.frame(model.matrix(fixed_formula, Xmat, ...))
    colnames(Xmat)[2:ncol(Xmat)] <- fixed_vars
    
    if(any(unlist(lapply(seq_along(sim_args[['fixed']]), function(xx) 
      sim_args[['fixed']][[xx]]$var_type)) == 'factor')) {
      Omat_factor <- Omat[unique_columns(Xmat, Omat)]
      Omat_factor_names <- attr(terms(fixed_formula), "term.labels")
      if(any(grepl(":|^I", Omat_factor_names))) {
        Omat_factor_names <- Omat_factor_names[-int_loc]
      }
     
      names(Omat_factor) <- paste0(Omat_factor_names[unique_columns(Xmat, Omat)],
                                   '_orig')
    } else {
      Omat_factor <- NULL
    }
    if(any(grepl("^ns|^poly", attr(terms(fixed_formula), "term.labels")))) {
      fixed_vars_new <- attr(terms(fixed_formula), "term.labels") 
      fixed_vars_poly_ns <- gsub("poly\\(|\\,.+\\)|ns\\(|\\,.+\\)", "", 
                                 fixed_vars_new[grepl("^poly|^ns", fixed_vars_new)]
      )
      Omat_poly_ns <- Omat[ , fixed_vars_poly_ns, drop = FALSE]
      names(Omat_poly_ns) <- paste0(names(Omat_poly_ns), "_orig")
    } else {
      Omat_poly_ns <- NULL
    }
    
    Omat <- dplyr::bind_cols(Omat_factor, Omat_poly_ns)
    
    Xmat <- dplyr::bind_cols(Xmat, Omat)
  } else {
    Xmat <- data.frame(model.matrix(fixed_formula, Xmat, ...))
    colnames(Xmat)[2:ncol(Xmat)] <- fixed_vars
  }
  
  if(is.null(data)) {
    data.frame(Xmat, ids)
  } else {
    data.frame(data, Xmat)
  }
}


#' Tidy heterogeneity of variance simulation
#' 
#' This function simulates heterogeneity of level one error variance.
#' 
#' @param data Data simulated from other functions to pass to this function. 
#' This function needs to be specified after `simulate_fixed` and `simulate_error`.
#' @param sim_args A named list with special model formula syntax. See details and examples
#'   for more information. The named list may contain the following:
#'   \itemize{
#'     \item fixed: This is the fixed portion of the model (i.e. covariates)
#'     \item random: This is the random portion of the model (i.e. random effects)
#'     \item error: This is the error (i.e. residual term).
#'   }
#' @param ... Other arguments to pass to error simulation functions.
#' @export
simulate_heterogeneity <- function(data, sim_args, ...) {
  
  heterogeneity_error <- heterogeneity(variance = sim_args[['heterogeneity']][['variance']],
                fixef = data, 
                variable = sim_args[['heterogeneity']][['variable']],
                err = data[['error']])

  data.frame(data[, !(names(data) %in% 'error')], 
             error = heterogeneity_error, orig_error = data[['error']])
  
}



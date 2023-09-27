#' Parses tidy formula simulation syntax
#' 
#' A function that parses the formula simulation syntax in order to simulate data.
#' 
#' @param sim_args A named list with special model formula syntax. See details and examples
#'   for more information. The named list may contain the following:
#'   \itemize{
#'     \item fixed: This is the fixed portion of the model (i.e. covariates)
#'     \item random: This is the random portion of the model (i.e. random effects)
#'     \item error: This is the error (i.e. residual term).
#'   }
#' 
#' @export
parse_formula <- function(sim_args) {
  
  outcome <- as.character(sim_args[['formula']])[2]
  
  fixed <- as.formula(paste0("~", gsub("^\\s+|\\s+$", "", gsub("\\+\\s*(\\s+|\\++)\\(.*?\\)", "", as.character(sim_args[['formula']])[3]))))
  
  randomeffect <- gsub("^\\s+|\\s+$", "", unlist(regmatches(as.character(sim_args[['formula']])[3], 
                                                            gregexpr("(\\+|\\s+)\\(.*?\\)", as.character(sim_args[['formula']])[3]))))
  
  list(outcome = outcome, 
       fixed = fixed,
       randomeffect = randomeffect)
}

#' Parses random effect specification
#' 
#' @param formula Random effect formula already parsed by \code{\link{parse_formula}}
#' 
#' @export 
parse_randomeffect <- function(formula) {
  
  cluster_id_vars <- gsub("^\\s+|\\s+$", "", gsub("\\)", "", unlist(lapply(seq_along(formula), function(xx) strsplit(formula, "\\|")[[xx]][2]))))
  
  random_effects <- paste0('~', gsub("^\\s+|\\s+$", "", gsub("\\(", "", unlist(lapply(seq_along(formula), function(xx) strsplit(formula, "\\|")[[xx]][1])))))
  
  list(
    cluster_id_vars = cluster_id_vars,
    random_effects = random_effects
  )

}

#' Parse Multiple Membership Random Effects
#' 
#' @param sim_args Simulation arguments
#' @param random_formula_parsed This is the output from 
#'   \code{\link{parse_randomeffect}}.
#' 
#' @export
parse_multiplemember <- function(sim_args, random_formula_parsed) {
  multiple_member_re <- lapply(seq_along(sim_args[['randomeffect']]), 
                           function(xx) 
                             sim_args[['randomeffect']][[xx]][['multiple_member']])
  multiple_member_re <- unlist(lapply(seq_along(multiple_member_re), function(xx)  
    !is.null(multiple_member_re[[xx]])))
  num_res <- lapply(lapply(seq_along(random_formula_parsed[['random_effects']]), 
                           function(xx) 
                             unlist(strsplit(random_formula_parsed[['random_effects']][xx], '\\+'))), 
                    length)
  num_res <- unlist(lapply(seq_along(num_res), function(xx) 
    rep(random_formula_parsed[['cluster_id_vars']][xx], num_res[[xx]])))
  
  multiple_member_idvars <- num_res[multiple_member_re]
  
  list(multiple_member_idvars = multiple_member_idvars,
       num_res = num_res,
       multiple_member_re = multiple_member_re
  )
}

#' Parse power specifications
#' 
#' @param sim_args A named list with special model formula syntax. See details and examples
#'   for more information. The named list may contain the following:
#'   \itemize{
#'     \item fixed: This is the fixed portion of the model (i.e. covariates)
#'     \item random: This is the random portion of the model (i.e. random effects)
#'     \item error: This is the error (i.e. residual term).
#'   }
#' @param samp_size The sample size pulled from the simulation arguments or the 
#'  power model results when vary_arguments is used.
#' @importFrom dplyr quo
#' @export 
parse_power <- function(sim_args, samp_size) {
  
  if(is.null(sim_args[['model_fit']][['reg_weights_model']])) {
    reg_weights <- sim_args[['reg_weights']]
  } else {
    reg_weights <- sim_args[['model_fit']][['reg_weights_model']]
  }
  
  if(is.null(sim_args[['power']][['direction']])) {
    number_tails <- rep(2, length(reg_weights))
  } else {
    number_tails <- ifelse(sim_args[['power']][['direction']] %in% c('lower', 'upper') == TRUE, 1, 2)
    if(length(number_tails) != length(reg_weights)) {
      number_tails <- rep(number_tails, length(reg_weights))
    }
  }
  
  if(is.null(sim_args[['power']][['direction']])) {
    tail_direction <- rep('two-tailed', length(reg_weights))
  } else {
    tail_direction <- sim_args[['power']][['direction']]
    if(length(tail_direction) != length(reg_weights)) {
      tail_direction <- rep(tail_direction, length(reg_weights))
    }
  }
  
  if(is.null(sim_args[['power']][['dist']])) {
    stat_dist <- rep('qnorm', length(reg_weights))
  } else {
    stat_dist <- sim_args[['power']][['dist']]
    if(length(stat_dist) != length(reg_weights)) {
      stat_dist <- rep(stat_dist, length(reg_weights))
    }
  }
  
  if(is.null(sim_args[['power']][['alpha']])) {
    alpha <- rep(0.05, length(reg_weights))
  } else {
    alpha <- sim_args[['power']][['alpha']]
    if(length(alpha) != length(reg_weights)) {
      alpha <- rep(alpha, length(reg_weights))
    }
  }
  
  lower_tail <- ifelse(tail_direction == 'lower', TRUE, FALSE)
  
  if(is.null(sim_args[['power']][['opts']])) {
    opts <- NULL
  } else {
    opts <- sim_args[['power']][['opts']]
    if(length(opts) != length(reg_weights)) {
      opts <- rep(opts, length(reg_weights))
    }
  }
  
  alpha <- alpha / number_tails
  
  test_statistic <- lapply(seq_along(stat_dist), function(ii) {
    if(is.null(sim_args[['power']][['opts']][['df']]) & stat_dist[ii] == 'qt') {
      df <- purrr::map(samp_size, `-`, 1)
      
      lapply(seq_along(df), function(xx) {
        purrr::exec(stat_dist[ii], 
                      p = alpha[ii], 
                      lower.tail = lower_tail[ii],
                      df = df[[xx]],
                      !!!opts[ii])
        # purrr::invoke(stat_dist[ii], 
        #               p = alpha[ii], 
        #               lower.tail = lower_tail[ii],
        #               df = df[[xx]],
        #               opts[ii])
      })
    } else {
      purrr::exec(stat_dist[ii], 
                    p = alpha[ii], 
                    lower.tail = lower_tail[ii],
                    !!!opts[ii])
      # purrr::invoke(stat_dist[ii], 
      #               p = alpha[ii], 
      #               lower.tail = lower_tail[ii],
      #               opts[ii])
    }
  }
  )
  

  lapply(seq_along(test_statistic), function(xx) {
    list(test_statistic = test_statistic[[xx]],
         alpha = alpha[xx], 
         number_tails = number_tails[xx],
         direction = tail_direction[xx],
         distribution = stat_dist[xx],
         opts = opts[xx]
    )
  })
  
}

#' Parse between varying arguments
#' 
#' @param sim_args A named list with special model formula syntax. See details and examples
#'   for more information. The named list may contain the following:
#'   \itemize{
#'     \item fixed: This is the fixed portion of the model (i.e. covariates)
#'     \item random: This is the random portion of the model (i.e. random effects)
#'     \item error: This is the error (i.e. residual term).
#'   }
#'   
#' @export
parse_varyarguments <- function(sim_args) {
  
  conditions <- expand.grid(list_select(sim_args[['vary_arguments']],
                                        names = c('model_fit', 'power'),
                                        exclude = TRUE), 
                            KEEP.OUT.ATTRS = FALSE)
  if(any(sapply(conditions, is.list))) {
    loc <- sapply(conditions, is.list)
    simp_conditions <- conditions[loc != TRUE]
    list_conditions <- conditions[loc == TRUE]
    list_conditions <- lapply(seq_along(list_conditions), function(xx) 
      unlist(list_conditions[xx], recursive = FALSE))
    for(tt in seq_along(list_conditions)) {
      names(list_conditions[[tt]]) <- gsub("[0-9]*", "", names(list_conditions[[tt]]))
    }
    lapply(1:nrow(conditions), function(xx) c(sim_args, 
                                              simp_conditions[xx, , drop = FALSE], 
                                              do.call('c', lapply(seq_along(list_conditions), function(tt) 
                                                list_conditions[[tt]][xx]))
    ))
  } else {
    lapply(1:nrow(conditions), function(xx) c(sim_args, 
                                              conditions[xx, , drop = FALSE]))
  }
  
}

#' Parse within varying arguments
#' 
#' @param sim_args A named list with special model formula syntax. See details and examples
#'   for more information. The named list may contain the following:
#'   \itemize{
#'     \item fixed: This is the fixed portion of the model (i.e. covariates)
#'     \item random: This is the random portion of the model (i.e. random effects)
#'     \item error: This is the error (i.e. residual term).
#'   }
#'   
#' @export
parse_varyarguments_w <- function(sim_args) {
  
  conditions <- expand.grid(list_select(sim_args[['vary_arguments']],
                                        names = c('model_fit'),
                                        exclude = FALSE), 
                            KEEP.OUT.ATTRS = FALSE)
  if(any(sapply(conditions, is.list))) {
    loc <- sapply(conditions, is.list)
    simp_conditions <- conditions[loc != TRUE]
    list_conditions <- conditions[loc == TRUE]
    list_conditions <- lapply(seq_along(list_conditions), function(xx) 
      unlist(list_conditions[xx], recursive = FALSE))
    for(tt in seq_along(list_conditions)) {
      names(list_conditions[[tt]]) <- gsub("[0-9]*", "", names(list_conditions[[tt]]))
    }
    lapply(1:nrow(conditions), function(xx) c(sim_args, 
                                              simp_conditions[xx, , drop = FALSE], 
                                              do.call('c', lapply(seq_along(list_conditions), function(tt) 
                                                list_conditions[[tt]][xx]))
    ))
  } else {
    lapply(1:nrow(conditions), function(xx) c(sim_args, 
                                              conditions[xx, , drop = FALSE]))
  }
  
}


#' Parse correlation arguments
#' 
#' This function is used to parse user specified correlation attributes. 
#' The correlation attributes need to be in a dataframe to be processed 
#' internally. Within the dataframe, there are expected to be 3 columns, 
#' 1) names of variable/attributes, 2) the variable/attribute pair for 1, 
#' 3) the correlation. 
#' 
#' @param sim_args A named list with special model formula syntax. See details and examples
#'   for more information. The named list may contain the following:
#'   \itemize{
#'     \item fixed: This is the fixed portion of the model (i.e. covariates)
#'     \item random: This is the random portion of the model (i.e. random effects)
#'     \item error: This is the error (i.e. residual term).
#'     \item correlate: These are the correlations for random effects and/or
#'        fixed effects.
#'   }
#' @importFrom gtools combinations
#'   
#' @export
parse_correlation <- function(sim_args) {
  
  fixed_formula <- parse_formula(sim_args)[['fixed']]
  
  fixed_vars <- attr(terms(fixed_formula), "term.labels") 
  
  if(!is.null(sim_args[['correlate']][['fixed']])) {
    if(length(fixed_vars) != nrow(sim_args[['correlate']][['fixed']])) {
      pairwise_options <- gtools::combinations(length(fixed_vars), 2, fixed_vars)
      
    }
  }
  
  fixed_correlation <- dataframe2matrix(sim_args[['correlate']][['fixed']],
                                        corr_variable = 'corr', 
                                        var_names = c('x', 'y'))
  
  random_correlation <- dataframe2matrix(sim_args[['correlate']][['random']],
                                         corr_variable = 'corr',
                                         var_names = c('x', 'y'))
  
  list(fixed_correlation = fixed_correlation,
       random_correlation = random_correlation)
}

parse_fixedtype <- function(sim_args, names) {
  
  lapply(names, function(xx) sim_args[['fixed']][[xx]][['var_type']])
  
}

parse_fixedlevels <- function(sim_args, names) {
  lapply(names, function(xx) sim_args[['fixed']][[xx]]['levels'])
}

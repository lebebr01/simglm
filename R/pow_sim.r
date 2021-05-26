#' Tidy Model Fitting Function
#' 
#' @param data A data object, most likely generated from within simglm
#' @param sim_args A named list with special model formula syntax. See details and examples
#'   for more information. The named list may contain the following:
#'   \itemize{
#'     \item fixed: This is the fixed portion of the model (i.e. covariates)
#'     \item random: This is the random portion of the model (i.e. random effects)
#'     \item error: This is the error (i.e. residual term).
#'     \item model_fit: These are arguments passed to the \code{\link{model_fit}}
#'       function. 
#'   }
#' @param ... Currently not used.
#' 
#' @export 
model_fit <- function(data, sim_args, ...) {
  
  model_args <- sim_args[['model_fit']]
  
  if(is.null(model_args[['model_function']])) {
    if(length(parse_formula(sim_args)[['randomeffect']]) == 0) {
      model_function <- 'lm'
      if(!is.null(sim_args[['outcome_type']])) {
        model_function <- 'glm'
      }
    } else {
      model_function <- 'lmer'
      if(!is.null(sim_args[['outcome_type']])) {
        model_function <- 'glmer'
      }
    }
  } else {
    model_function <- model_args[['model_function']]
  }
  
  if('id' %in% names(model_args)) {
    model_args[['id']] <- data[[model_args[['id']]]]
  }
  
  if(is.null(model_args[['formula']])) {
    model_args[['formula']] <- sim_args[['formula']]
  }
  
  model_args[['model_function']] <- NULL
  
  purrr::invoke(model_function, 
                model_args, 
                data = data)
}

#' Extract Coefficients
#' 
#' @param model A returned model object from a fitted model.
#' @param extract_function A function that extracts model results. The 
#'   function must take the model object as the only argument.
#' @export 
extract_coefficients <- function(model, extract_function = NULL) {
  
  if(any(c('glmerMod', 'lmerMod') %in% class(model))) {
    tidy_mixed(model)
  } else {
    if(is.null(extract_function)) {
      broom::tidy(model)
    } else {
      purrr::invoke(extract_function, model)
    }
  }
  
}

#' @importFrom methods selectMethod
tidy_mixed <- function(model) {
  
  sum_fun <- methods::selectMethod("summary", class(model))
  ss <- sum_fun(model)
  mod_results <- stats::coef(ss) %>% data.frame(check.names=FALSE)
  mod_results <- data.frame(term = rownames(mod_results), mod_results,
                            row.names = NULL)
  
  if(class(model) == 'glmerMod') {
   mod_results <- mod_results[, 1:4]
  }
  
  names(mod_results) <- c('term', 'estimate', 'std.error', 'statistic')
  
  mod_results
  
}



#' Replicate Simulation
#' 
#' @param sim_args A named list with special model formula syntax. See details and examples
#'   for more information. The named list may contain the following:
#'   \itemize{
#'     \item fixed: This is the fixed portion of the model (i.e. covariates)
#'     \item random: This is the random portion of the model (i.e. random effects)
#'     \item error: This is the error (i.e. residual term).
#'   }
#' @param return_list TRUE/FALSE indicating whether a full list output should be
#'   returned. If TRUE, the nested list is returned. If FALSE, replications are 
#'   combined with a replication id appended.
#' @param future.seed TRUE/FALSE or numeric. Default value is true, see 
#'   \code{\link[future.apply:future_lapply]{future_replicate}}.
#' @param ... Currently not used.
#' @importFrom future.apply future_replicate
#' @export 
replicate_simulation <- function(sim_args, return_list = FALSE, 
                                 future.seed = TRUE, ...) {
  
  if(is.null(sim_args[['vary_arguments']])) {
    future.apply::future_replicate(sim_args[['replications']], 
                                   simglm(sim_args),
                                   simplify = FALSE,
                                   future.seed = future.seed)
  } else {
    replicate_simulation_vary(sim_args, return_list = FALSE,
                              future.seed = future.seed)
  }
  
}

replicate_simulation_vary <- function(sim_args, return_list = FALSE,
                                      future.seed = TRUE) {
  
  conditions <- data.frame(sapply(expand.grid(sim_args[['vary_arguments']], KEEP.OUT.ATTRS = FALSE),
                                  as.character))
  
  sim_arguments <- parse_varyarguments(sim_args)
  
  power_out <- lapply(seq_along(sim_arguments), function(xx) 
          future.apply::future_replicate(sim_arguments[[xx]][['replications']], 
                                         simglm(sim_arguments[[xx]]),
                                         simplify = FALSE,
                                         future.seed = future.seed)
            )
  
  if(return_list) {
    return(power_out)
  } else {
    
    power_df <- lapply(power_out, dplyr::bind_rows)
    
    num_rows <- unlist(lapply(power_df, nrow))
    
    rep_id <- lapply(seq_along(num_rows), function(xx) 
      rep(1:sim_args[['replications']], 
          each = num_rows[xx]/sim_args[['replications']]))
    
    power_list <- lapply(seq_along(sim_arguments), function(xx) 
      data.frame(conditions[xx, , drop = FALSE],
                 replication = rep_id[[xx]],
                 power_df[[xx]],
                 row.names = NULL
      )
    )
    
    power_list
  }
}

#' Compute Power, Type I Error, or Precision Statistics
#' 
#' @param data A list of model results generated by \code{\link{replicate_simulation}}
#'  function.
#' @param sim_args A named list with special model formula syntax. See details and examples
#'   for more information. The named list may contain the following:
#'   \itemize{
#'     \item fixed: This is the fixed portion of the model (i.e. covariates)
#'     \item random: This is the random portion of the model (i.e. random effects)
#'     \item error: This is the error (i.e. residual term).
#'   }
#' @param power TRUE/FALSE flag indicating whether power should be computed. 
#'  Defaults to TRUE.
#' @param type_1_error TRUE/FALSE flag indicating whether type I error rate
#'  should be computed. Defaults to TRUE.
#' @param precision TRUE/FALSE flag indicating whether precision should be 
#'  computed. Defaults to TRUE.
#' @importFrom dplyr mutate
#' @importFrom rlang syms
#' @export
compute_statistics <- function(data, sim_args, power = TRUE, 
                               type_1_error = TRUE, precision = TRUE) {
  
  if(is.null(sim_args[['sample_size']])) {
    samp_size <- lapply(seq_along(data), function(xx) {
      unique(as.numeric(as.character(data[[xx]][['sample_size']])))
      })
  } else {
    samp_size <- sim_args[['sample_size']]
  }
  
  power_args <- parse_power(sim_args, samp_size)
  
  data_df <- data %>%
    Map(compute_power, ., power_args) %>%
    Map(compute_t1e, ., list(sim_args), 
                         power_args) %>%
    dplyr::bind_rows()
  
  # data_df <- data %>%
  #   purrr::map(compute_power, power_args) %>% 
  #   purrr::map(compute_t1e, sim_args = sim_args, t1e_args = power_args) %>%
  #   dplyr::bind_rows()
  
  if(is.null(sim_args['vary_arguments'])) {
    group_vars <- c('term')
  } else {
    group_vars <- c(names(expand.grid(sim_args[['vary_arguments']], KEEP.OUT.ATTRS = FALSE)),
                       'term')
  }
  
  avg_estimates <- aggregate_estimate(data_df,
                                      rlang::syms(group_vars))
  
  if(power) {
    power_computation <- aggregate_power(data_df, 
                                         rlang::syms(group_vars))
    avg_estimates <- dplyr::full_join(avg_estimates, 
                     power_computation,
                     by = 'term')
  }
  
  if(type_1_error) {
    type_1_error_computation <- aggregate_t1e(data_df, 
                                              rlang::syms(group_vars))
    avg_estimates <- dplyr::full_join(avg_estimates, 
                                      type_1_error_computation,
                                      by = 'term')
  }
  
  if(precision) {
    precision_computation <- aggregate_precision(data_df, 
                                                 rlang::syms(group_vars))
    avg_estimates <- dplyr::full_join(avg_estimates, 
                                      precision_computation,
                                      by = 'term')
  }
  
  avg_estimates['replications'] <- sim_args['replications']
  
  avg_estimates
  
}

compute_power <- function(data, power_args) {
  
  # power_args <- parse_power(sim_args)
  
  if(power_args['direction'] == 'lower') {
    data %>%
      mutate(reject = ifelse(statistic <= power_args['test_statistic'], 1, 0),
             test_statistic = power_args[['test_statistic']]) 
  } else {
    if(power_args['direction'] == 'upper') {
      data %>%
        mutate(reject = ifelse(statistic >= power_args['test_statistic'], 1, 0),
               test_statistic = power_args[['test_statistic']]) 
    } else {
      data %>%
        mutate(reject = ifelse(abs(statistic) >= power_args['test_statistic'], 1, 0),
               test_statistic = power_args[['test_statistic']]) 
    }
  }
}

compute_t1e <- function(data, sim_args, t1e_args) {
  
  fixed_vars <- strsplit(as.character(parse_formula(sim_args)[['fixed']]), "\\+")[[2]]
  
  if(is.null(sim_args[['reg_weights_model']])) {
    reg_weights <- sim_args[['reg_weights']]
  } else {
    reg_weights <- sim_args[['reg_weights_model']]
  }
  
  # if(length(fixed_vars) != length(reg_weights)) {
  #   stop("Check reg_weights in model_fit simulation arguments, must specify 
  #        reg_weights if specifying model")
  # }
  
  if(t1e_args['direction'] == 'lower') {
    data %>%
      mutate(adjusted_teststat = (estimate - reg_weights) / std.error,
             t1e = ifelse(adjusted_teststat <= t1e_args['test_statistic'], 
                          1, 0))
  } else {
    if(t1e_args['direction'] == 'upper') {
      data %>%
        mutate(adjusted_teststat = (estimate - reg_weights) / std.error,
               t1e = ifelse(adjusted_teststat >= t1e_args['test_statistic'], 
                            1, 0))
    } else {
      data %>%
        mutate(adjusted_teststat = (estimate - reg_weights) / std.error,
               t1e = ifelse(abs(adjusted_teststat) >= t1e_args['test_statistic'], 
                            1, 0))
    }
  }
}

aggregate_estimate <- function(data, group_var) {
  
  group_by_var <- dplyr::quos(!!! group_var)
  
  data %>%
    group_by(!!! group_by_var) %>%
    summarise(avg_estimate = mean(estimate))
}

aggregate_power <- function(data, group_var) {
  
  group_by_var <- dplyr::quos(!!! group_var)
  
  data %>%
    group_by(!!! group_by_var) %>%
    summarise(power = mean(reject),
              avg_test_stat = mean(statistic),
              crit_value = unique(test_statistic))
}

aggregate_t1e <- function(data, group_var) {
  
  group_by_var <- dplyr::quos(!!! group_var)
  
  data %>%
    group_by(!!! group_by_var) %>% 
    summarise(type_1_error = mean(t1e),
              avg_adjtest_stat = mean(adjusted_teststat),
              crit_value = unique(test_statistic))

}


aggregate_precision <- function(data, group_var) {
  
  group_by_var <- dplyr::quos(!!! group_var) 
  
  data %>%
    group_by(!!! group_by_var) %>% 
    summarise(param_estimate_sd = sd(estimate),
              avg_standard_error = mean(std.error),
              precision_ratio = param_estimate_sd / avg_standard_error)
}

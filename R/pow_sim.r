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
  
  if(!is.null(model_args[['reg_weights_model']])) {
    model_args[['reg_weights_model']] <- NULL
  }
  
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
  
  purrr::exec(model_function, 
                !!!model_args, 
                data = data)
  
  # purrr::invoke(model_function, 
  #               model_args, 
  #               data = data)
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

#' @importFrom methods selectMethod is
tidy_mixed <- function(model) {
  
  sum_fun <- methods::selectMethod("summary", class(model))
  ss <- sum_fun(model)
  mod_results <- stats::coef(ss) |> data.frame(check.names=FALSE)
  mod_results <- data.frame(term = rownames(mod_results), mod_results,
                            row.names = NULL)
  
  if(is(model, 'glmerMod')) {
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
    
    if(is.null(sim_args[['replications']])){
      sim_args[['replications']] <- 1
    }
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
  
  if(is.null(sim_args[['replications']])){
    sim_args[['replications']] <- 1
  }
  
  conditions <- data.frame(sapply(expand.grid(sim_args[['vary_arguments']], KEEP.OUT.ATTRS = FALSE),
                                  as.character))
  
  sim_arguments <- parse_varyarguments(sim_args)
  
  power_out <- future.apply::future_lapply(seq_along(sim_arguments), function(xx) {
          future.apply::future_replicate(sim_arguments[[xx]][['replications']], 
                                         simglm(sim_arguments[[xx]]),
                                         simplify = FALSE,
                                         future.seed = future.seed)
            }, future.seed = future.seed)
  
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
#' @param alternative_power TRUE/FALSE flag indicating whether alternative 
#'  power estimates should be computed. If TRUE, this must be accompanied by 
#'  thresholds specified within the power simulation arguments. Defaults to FALSE.
#' @param type_s_error TRUE/FALSE flag indicating whether Type S error should
#'  be computed. Defaults to FALSE.
#' @param type_m_error TRUE/FALSE flag indicating whether Type M error should
#'  be computed. Defaults to FALSE.
#' @importFrom dplyr mutate summarise group_by
#' @importFrom rlang syms
#' @export
compute_statistics <- function(data, sim_args, power = TRUE, 
                               type_1_error = TRUE, precision = TRUE,
                               alternative_power = FALSE,
                               type_s_error = FALSE,
                               type_m_error = FALSE) {
  
  if(is.null(sim_args[['sample_size']])) {
    samp_size <- lapply(seq_along(data), function(xx) {
      unique(as.numeric(as.character(data[[xx]][['sample_size']])))
      })
  } else {
    samp_size <- sim_args[['sample_size']]
  }
  
  power_args <- parse_power(sim_args, samp_size)
  
  if(is.null(sim_args[['model_fit']][['reg_weights_model']])) {
    reg_weights <- sim_args[['reg_weights']]
  } else {
    reg_weights <- sim_args[['model_fit']][['reg_weights_model']]
  }
  
  data_df <- do.call("rbind", data)
  
  data_list <- split(data_df, f = data_df['term'])
  
  data_list <- lapply(seq_along(data_list), function(xx) {
    compute_power(data_list[[xx]], power_args[[xx]])
    })
  data_list <- lapply(seq_along(data_list), function(xx) {
    compute_t1e(data_list[[xx]], power_args[[xx]], reg_weights = reg_weights[xx])
  })
  
  data_df <- do.call("rbind", data_list)
  
  if(is.null(sim_args['vary_arguments'])) {
    group_vars <- c('term')
  } else {
    group_vars <- c(names(expand.grid(sim_args[['vary_arguments']], KEEP.OUT.ATTRS = FALSE)),
                    'term')
  }
  
  avg_estimates <- aggregate_estimate(data_df,
                                      rlang::syms(group_vars))
  
  if(alternative_power) {
    alt_power_est <- alternative_power(data_df,
                                       group_var = group_vars,
                                       quantiles = sim_args[['power']][['thresholds']])
    avg_estimates <- dplyr::full_join(avg_estimates, 
                                      alt_power_est,
                                      by = group_vars)
  }
  
  if(type_s_error) {
    type_s <- type_s_errors(data_df, 
                              group_var = group_vars, 
                              sign = sim_args[['power']][['type_s_sign']])
    
    avg_estimates <- dplyr::full_join(avg_estimates, 
                                      type_s,
                                      by = group_vars)
  }
  
  # if(type_m_error){
  #   type_m <- type_m_s_errors(data_df, 
  #                             group_var = group_vars, 
  #                             sign = sim_args[['power']][['type_m_threshold']])
  #   
  #   avg_estimates <- dplyr::full_join(avg_estimates, 
  #                                     type_m,
  #                                     by = group_vars)
  # }
  
  if(power) {
    power_computation <- aggregate_power(data_df, 
                                         rlang::syms(group_vars))
    avg_estimates <- dplyr::full_join(avg_estimates, 
                     power_computation,
                     by = group_vars)
  }
  
  if(type_1_error) {
    type_1_error_computation <- aggregate_t1e(data_df, 
                                              rlang::syms(group_vars))
    avg_estimates <- dplyr::full_join(avg_estimates, 
                                      type_1_error_computation,
                                      by = group_vars)
  }
  
  if(precision) {
    precision_computation <- aggregate_precision(data_df, 
                                                 rlang::syms(group_vars))
    avg_estimates <- dplyr::full_join(avg_estimates, 
                                      precision_computation,
                                      by = group_vars)
  }
  
  avg_estimates['replications'] <- sim_args['replications']
  
  avg_estimates
  
}

compute_power <- function(data, power_args) {
  
  # power_args <- parse_power(sim_args)
  
  if(power_args['direction'] == 'lower') {
    data |>
      mutate(reject = ifelse(statistic <= power_args['test_statistic'], 1, 0),
             test_statistic = power_args[['test_statistic']]) 
  } else {
    if(power_args['direction'] == 'upper') {
      data |>
        mutate(reject = ifelse(statistic >= power_args['test_statistic'], 1, 0),
               test_statistic = power_args[['test_statistic']]) 
    } else {
      data |>
        mutate(reject = ifelse(abs(statistic) >= power_args['test_statistic'], 1, 0),
               test_statistic = power_args[['test_statistic']]) 
    }
  }
}

compute_t1e <- function(data, t1e_args, reg_weights) {
  
  # fixed_vars <- strsplit(as.character(parse_formula(sim_args)[['fixed']]), "\\+")[[2]]
  # 
  # if(is.null(sim_args[['model_fit']][['reg_weights_model']])) {
  #   reg_weights <- sim_args[['reg_weights']]
  # } else {
  #   reg_weights <- sim_args[['model_fit']][['reg_weights_model']]
  # }
  
  # if(length(fixed_vars) != length(reg_weights)) {
  #   stop("Check reg_weights in model_fit simulation arguments, must specify 
  #        reg_weights if specifying model")
  # }
  
  if(t1e_args['direction'] == 'lower') {
    data |>
      mutate(adjusted_teststat = (estimate - reg_weights) / std.error,
             t1e = ifelse(adjusted_teststat <= t1e_args['test_statistic'], 
                          1, 0))
  } else {
    if(t1e_args['direction'] == 'upper') {
      data |>
        mutate(adjusted_teststat = (estimate - reg_weights) / std.error,
               t1e = ifelse(adjusted_teststat >= t1e_args['test_statistic'], 
                            1, 0))
    } else {
      data |>
        mutate(adjusted_teststat = (estimate - reg_weights) / std.error,
               t1e = ifelse(abs(adjusted_teststat) >= t1e_args['test_statistic'], 
                            1, 0))
    }
  }
}

aggregate_estimate <- function(data, group_var) {
  
  group_by_var <- dplyr::quos(!!! group_var)
  
  data |>
    group_by(!!! group_by_var) |>
    summarise(avg_estimate = mean(estimate))
}

aggregate_power <- function(data, group_var) {
  
  group_by_var <- dplyr::quos(!!! group_var)
  
  data |>
    group_by(!!! group_by_var) |>
    summarise(power = mean(reject),
              avg_test_stat = mean(statistic),
              crit_value_power = unique(test_statistic))
}

aggregate_t1e <- function(data, group_var) {
  
  group_by_var <- dplyr::quos(!!! group_var)
  
  data |>
    group_by(!!! group_by_var) |>
    summarise(type_1_error = mean(t1e),
              avg_adjtest_stat = mean(adjusted_teststat),
              crit_value_t1e = unique(test_statistic))

}

#' @importFrom stats sd aggregate as.formula model.matrix rbinom rnorm rpois runif terms
aggregate_precision <- function(data, group_var) {
  
  group_by_var <- dplyr::quos(!!! group_var) 
  
  data |>
    group_by(!!! group_by_var) |>
    summarise(param_estimate_sd = sd(estimate),
              avg_standard_error = mean(std.error),
              precision_ratio = param_estimate_sd / avg_standard_error)
}

alternative_power <- function(data, group_var, 
                              quantiles) {
  
  data_list <- split(data, f = data[group_var])
  
  alt_power_out <- lapply(seq_along(data_list), function(ii) {
    do.call("rbind", lapply(seq_along(quantiles[[ii]]), function(xx) {
      c(compute_alt_power(data_list[[ii]], quantile = quantiles[[ii]][xx]),
      names(data_list)[[ii]]) }
  ))}
  )
  
 alt_power_df <- data.frame(do.call("rbind", alt_power_out))
 names(alt_power_df) <- c('alt_power', 'threshold', group_var)
 
 alt_power_df
}

compute_alt_power <- function(data, quantile) {
  
  if(quantile < 0) {
    c(mean(ifelse(data[['estimate']] <= quantile, 1, 0)), quantile)
  } else {
    c(mean(ifelse(data[['estimate']] >= quantile, 1, 0)), quantile)
  }
}

type_s_errors <- function(data, group_var, sign = NULL) {
  
  data_list <- split(data, f = data[group_var])
  
  if(!is.null(sign)) {
    type_s <- data.frame(do.call("rbind", lapply(seq_along(data_list), function(ii) {
      c(compute_type_s(data_list[[ii]], sign[ii]), names(data_list)[[ii]])
    })))
    names(type_s) <- c('type_s_error', 'sign', group_var)
  }
  type_s
}

compute_type_s <- function(data, sign) {
  
  if(sign == 'positive') {
    c(mean(ifelse(data[['estimate']] < 0, 1, 0)), sign)
  } else {
    c(mean(ifelse(data[['estimate']] > 0, 1, 0)), sign)
  }
}

compute_type_m <- function(data, quantile) {
  
}

#' Convenience function for computing density values for plotting.
#' 
#' @param data A dataframe that contains the parameter estimates.
#' @param group_var A group variable that specifies the attributes to 
#' group by. By default, this would likely be the term attribute, but can 
#' contain more than one attribute.
#' @param parameter The attribute that represents the parameter estimate.
#' @param values A list of numeric vectors that specifies the values 
#' for which the density values are computed for.
#' 
#' @export
#' @importFrom stats density
compute_density_values <- function(data, group_var, parameter,
                                   values) {
  
  data_list <- data |>
    split(f = data[group_var]) 

  dens_values <- future.apply::future_lapply(seq_along(data_list), function(ii) 
    cbind(density_quantile(data_list[[ii]][[parameter]], 
         quantiles = values[[ii]]), 
         names(data_list)[[ii]]), future.seed = TRUE
    )
  dens_df <- do.call('rbind', dens_values)
  names(dens_df) <- c('x', 'y', group_var)
  
  dens_df
}
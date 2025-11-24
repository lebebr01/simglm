#' Correlate elements
#'
#' @param data Data simulated from other functions to pass to this function.
#' @param sim_args A named list with special model formula syntax. See details and examples
#'   for more information. The named list may contain the following:
#'   \itemize{
#'     \item fixed: This is the fixed portion of the model (i.e. covariates)
#'     \item random: This is the random portion of the model (i.e. random effects)
#'     \item error: This is the error (i.e. residual term).
#'     \item correlate: These are the correlations for random effects and/or
#'        fixed effects.
#'   }
#' @param ... Additional arguments, currently not used.
#'
#' @export
correlate_variables <- function(data, sim_args, ...) {
  correlation_matrices <- parse_correlation(sim_args)

  if (!is.null(correlation_matrices[['random_correlation']])) {
    data <- correlate_randomeffects(data, sim_args, correlation_matrices)
  }
  if (!is.null(correlation_matrices[['fixed_correlation']])) {
    data <- correlate_fixedeffects(data, sim_args, correlation_matrices)
  }

  data
}

correlation2covariance <- function(correlation, sd) {
  diag(sd) %*%
    correlation %*%
    diag(sd)
}

correlate_attributes <- function(data, covariance, sd, mean, var_names) {
  es <- eigen(covariance, symmetric = TRUE)
  ev <- es[['values']]

  corr_data <- data.frame(t(
    mean + es[['vectors']] %*% diag(sqrt(pmax(ev, 0)), length(sd)) %*% t(data)
  ))

  names(corr_data) <- var_names

  corr_data
}

correlate_randomeffects <- function(data, sim_args, correlation_matrices) {
  sd_vars <- sqrt(unlist(lapply(
    seq_along(sim_args[['randomeffect']]),
    function(xx) {
      sim_args[['randomeffect']][[xx]][['variance']]
    }
  )))
  mean_vars <- unlist(lapply(
    seq_along(sim_args[['randomeffect']]),
    function(xx) {
      sim_args[['randomeffect']][[xx]][['mean']]
    }
  ))
  if (is.null(mean_vars)) {
    mean_vars <- rep(0, length(sd_vars))
  }

  var_names <- names(sim_args[['randomeffect']])[
    names(sim_args[['randomeffect']]) %in%
      colnames(correlation_matrices[['random_correlation']])
  ]

  correlate_data <- data[colnames(correlation_matrices[['random_correlation']])]
  correlate_data <- do.call(
    'cbind',
    lapply(seq_along(sd_vars), function(xx) {
      standardize(correlate_data[[xx]], mean = mean_vars[xx], sd = sd_vars[xx])
    })
  )

  covariance <- correlation2covariance(
    correlation_matrices[['random_correlation']],
    sd = sd_vars
  )

  correlated_data <- correlate_attributes(
    correlate_data,
    covariance = covariance,
    sd = sd_vars,
    mean = mean_vars,
    var_names = var_names
  )

  names(data)[names(data) %in% var_names] <- paste0(var_names, '_old')

  cbind(data, correlated_data)
}

correlate_fixedeffects <- function(data, sim_args, correlation_matrices) {
  sd_vars <- unlist(lapply(seq_along(sim_args[['fixed']]), function(xx) {
    sim_args[['fixed']][[xx]][['sd']]
  }))
  mean_vars <- unlist(lapply(seq_along(sim_args[['fixed']]), function(xx) {
    sim_args[['fixed']][[xx]][['mean']]
  }))
  if (is.null(mean_vars)) {
    mean_vars <- rep(0, length(sd_vars))
  }

  var_names <- names(sim_args[['fixed']])[
    names(sim_args[['fixed']]) %in%
      colnames(correlation_matrices[['fixed_correlation']])
  ]

  if (any(unlist(parse_fixedtype(sim_args, var_names)) == 'ordinal')) {
    ordinal_loc <- unlist(parse_fixedtype(sim_args, var_names)) == 'ordinal'

    sd_ordinal <- lapply(var_names[ordinal_loc], function(xx) {
      sd(data[[xx]], na.rm = TRUE)
    })
    mean_ordinal <- lapply(var_names[ordinal_loc], function(xx) {
      mean(data[[xx]], na.rm = TRUE)
    })

    ordinal_loc_grep <- grep("TRUE", ordinal_loc) - 1

    for (i in 0:(length(ordinal_loc_grep) - 1)) {
      sd_vars <- append(
        sd_vars,
        sd_ordinal[[i + 1]],
        after = (ordinal_loc_grep[i + 1] + i)
      )
      mean_vars <- append(
        mean_vars,
        mean_ordinal[[i + 1]],
        after = (ordinal_loc_grep[i + 1] + i)
      )
    }
  }

  correlate_data <- data[colnames(correlation_matrices[['fixed_correlation']])]
  correlate_data <- do.call(
    'cbind',
    lapply(seq_along(sd_vars), function(xx) {
      standardize(correlate_data[[xx]], mean = mean_vars[xx], sd = sd_vars[xx])
    })
  )

  covariance <- correlation2covariance(
    correlation_matrices[['fixed_correlation']],
    sd = sd_vars
  )

  correlated_data <- correlate_attributes(
    correlate_data,
    covariance = covariance,
    sd = sd_vars,
    mean = mean_vars,
    var_names = var_names
  )

  if (any(unlist(parse_fixedtype(sim_args, var_names)) == 'ordinal')) {
    ordinal_loc <- unlist(parse_fixedtype(sim_args, var_names)) == 'ordinal'

    ordinal_levels <- parse_fixedlevels(sim_args, var_names[ordinal_loc])
    ordinal_min <- lapply(seq_along(ordinal_levels), function(xx) {
      min(unlist(ordinal_levels[[xx]]))
    })
    ordinal_max <- lapply(seq_along(ordinal_levels), function(xx) {
      max(unlist(ordinal_levels[[xx]]))
    })

    round_correlated_data <- do.call(
      "cbind",
      lapply(seq_along(var_names[ordinal_loc]), function(xx) {
        round_ordinal(
          correlated_data[[var_names[ordinal_loc][xx]]],
          min = ordinal_min[[xx]],
          max = ordinal_max[[xx]]
        )
      })
    ) |>
      data.frame()
    names(round_correlated_data) <- var_names[ordinal_loc]
    names(correlated_data)[
      names(correlated_data) %in% var_names[ordinal_loc]
    ] <- paste0(var_names[ordinal_loc], '_corr')
    correlated_data <- cbind(correlated_data, round_correlated_data)
  }

  names(data)[names(data) %in% var_names] <- paste0(var_names, '_old')

  cbind(data, correlated_data)
}

round_ordinal <- function(variable, min, max) {
  rounded_data <- round(variable, 0)
  rounded_data <- ifelse(
    rounded_data < min,
    min,
    ifelse(rounded_data > max, max, rounded_data)
  )

  rounded_data
}

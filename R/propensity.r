#' Simulate Propensity Scores
#'
#' Process to simulate propensity scores.
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
simulate_propensity <- function(sim_args) {
  if (is.null(sim_args[['propensity']])) {
    stop('Simulation arguments must specify propensity arguments')
  }

  if (is.null(sim_args[['propensity']][['sample_size']])) {
    sim_args[['propensity']][['sample_size']] <-
      sim_args[['sample_size']]
  }

  simglm(sim_args[['propensity']])
}

#' Primary propensity model fitting
#'
#' Incorporates the estimated probabilities and weights.
#'
#' @param data Data object that contains the simulated data.
#' @param sim_args The simulation argument list that contains the propensity
#'  score arguments needed.
#'
#' @return Propensity probabilities and weights if specified.
#' @export
fit_propensity <- function(data, sim_args) {
  if (!is.null(sim_args[['propensity_model']])) {
    propensity_scores <- extract_propensity(data = data, sim_args = sim_args)
    prop_data <- cbind(data, propensity = propensity_scores)

    if (
      sim_args[['propensity_model']][['propensity_type']] %in% c('ipw', 'sbw')
    ) {
      propensity_weights <- propensity_weights(
        data = prop_data,
        sim_args = sim_args
      )
      prop_data <- cbind(prop_data, weights = propensity_weights)
    }
  }
  prop_data
}

#' @importFrom stats fitted formula glm update terms
extract_propensity <- function(data, sim_args) {
  # Resolve formula
  formula <- sim_args[['propensity_model']][['formula']] %||%
    sim_args[['propensity']][['formula']]
  if (is.null(formula)) {
    stop("Must specify a propensity model formula")
  }

  # Resolve model fitting function (default = stats::glm)
  fit_fun <- sim_args[['propensity_model']][['model_function']]
  if (is.null(fit_fun)) {
    fit_fun <- stats::glm
  } else if (is.character(fit_fun)) {
    # Optional string-based dispatch for common cases
    if (fit_fun == "glm") {
      fit_fun <- stats::glm
    } else if (fit_fun == "glmer") {
      if (!requireNamespace("lme4", quietly = TRUE)) {
        stop(
          "Package 'lme4' required for glmer model_function but not installed."
        )
      }
      fit_fun <- lme4::glmer
    } else {
      stop("Unknown model_function: ", fit_fun)
    }
  }

  # Collect user-specified additional arguments
  fit_args <- sim_args[['propensity_model']][['propensity_model_args']] %||%
    list()

  # Add required arguments (formula, data)
  fit_args <- c(list(formula = formula, data = data), fit_args)

  # Fit model
  model <- do.call(fit_fun, fit_args)

  # Extract fitted values
  fitted(model)
}


propensity_weights <- function(data, sim_args) {
  prop_response <- attr(
    terms(sim_args[['propensity_model']][['formula']]),
    "response"
  )

  if (sim_args[['propensity_model']][['propensity_type']] == 'ipw') {
    # Calculate IPW weights
    weights <- ifelse(
      data[[prop_response]] == 1,
      1 / data[['propensity']],
      1 / (1 - data[['propensity']])
    )
  }

  if (sim_args[['propensity_model']][['propensity_type']] == 'sbw') {
    # Stabilized weights
    p_treatment <- mean(data[[prop_response]])
    weights <- ifelse(
      data[[prop_response]] == 1,
      p_treatment / data[['propensity']],
      (1 - p_treatment) / (1 - data[['propensity']])
    )
  }
  weights
}

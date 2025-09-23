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

  simglm(sim_args[['propensity']])
}

extract_propensity <- function(data, sim_args) {
  if (is.null(sim_args[['propensity_model']][['formula']])) {
    if (is.null(sim_args[['propensity']][['formula']])) {
      stop("Must specify a propensity model formula")
    }
    sim_args[['propensity_model']][['formula']] <- sim_args[['propensity']][[
      'formula'
    ]]
  }

  if (length(parse_formula(sim_args)[['propensity_model']][['formula']]) == 0) {
    glm(
      sim_args[['propensity_model']][['formula']],
      data = data,
      family = 'binomial'
    ) |>
      fitted()
  } else {
    glmer(
      sim_args[['propensity_model']][['formula']],
      data = data,
      family = 'binomial'
    ) |>
      fitted()
  }
}

propensity_weights <- function(data, sim_args) {
  prop_response <- attr(
    terms(sim_args[['propensity_model']][['formula']]),
    "response"
  )

  if (sim_args[['propensity_model']][['propensity_type']] == 'ipw') {
    # Calculate IPW weights
    ifelse(
      data[[prop_response]] == 1,
      1 / data[['propensity']],
      1 / (1 - data[['propensity']])
    )
  }

  if (sim_args[['propensity_model']][['propensity_type']] == 'sbw') {
    # Stabilized weights
    p_treatment <- mean(data[[prop_response]])
    ifelse(
      data[[prop_response]] == 1,
      p_treatment / data[['propensity']],
      (1 - p_treatment) / (1 - data[['propensity']])
    )
  }
}

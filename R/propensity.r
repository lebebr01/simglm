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

#' Function to fit initial propensity score model
#'
#' After simulation the logistic regression propensity score model.
#'
#' @param data Simulated data to pass to the function.
#' @param sim_args A named list with special model formula syntax. See details and examples
#'   for more information. The named list may contain the following:
#'   \itemize{
#'     \item fixed: This is the fixed portion of the model (i.e. covariates)
#'     \item random: This is the random portion of the model (i.e. random effects)
#'     \item error: This is the error (i.e. residual term).
#'   }
#' @export
fit_propensity <- function(data, sim_args) {
  # switch(
  #   var_type,
  #   covariate = fit_propensity_covariate(data = data, sim_args = sim_args)
  #   ipw = fit_propensity_ipw(data = data, sim_args = sim_args)
  # )
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

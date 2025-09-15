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

  simglm(sim_args)
}

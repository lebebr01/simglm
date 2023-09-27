#' Single wrapper function
#' 
#' This function is most useful to pass to \code{\link{replicate_simulation}}.
#'   The function attempts to determine automatically which aspects to add to 
#'   the simulation/power generation based on the elements found in the sim_args
#'   argument.
#' 
#' @param sim_args A named list with special model formula syntax. See details and examples
#'   for more information. The named list may contain the following:
#'   \itemize{
#'     \item fixed: This is the fixed portion of the model (i.e. covariates)
#'     \item random: This is the random portion of the model (i.e. random effects)
#'     \item error: This is the error (i.e. residual term).
#'   }
#' @export
simglm <- function(sim_args) {
  
  if(is.null(sim_args[['formula']])) {
    stop('Simulation arguments must have a formula')
  }
  if(is.null(sim_args[['reg_weights']])) {
    stop('Simulation arguments must have regression weights')
  }
  if(is.null(sim_args[['sample_size']])) {
    stop('Simulation arguments must specify sample size')
  }
  
  data <- simulate_fixed(data = NULL, sim_args = sim_args)
  
  if(!is.null(sim_args[['error']])) {
    data <- simulate_error(data, sim_args = sim_args)
  }
  
  if(!is.null(sim_args[['heterogeneity']])) {
    data <- simulate_heterogeneity(data, sim_args = sim_args)
  }
  
  if(!is.null(sim_args[['randomeffect']])) {
    data <- simulate_randomeffect(data, sim_args = sim_args)
  }
  
  if(!is.null(sim_args[['correlate']])) {
    data <- correlate_variables(data, sim_args = sim_args)
  }
  
  data <- generate_response(data, sim_args = sim_args)
  
  if(!is.null(sim_args[['missing_data']])) {
    data <- generate_missing(data, sim_args = sim_args)
  }
  
  data
  
}

simglm_modelfit <- function(data, sim_args) {
  if(is.null(data)) {
    error('Must pass a valid data object')
  }
  
  if(!is.null(sim_args[['model_fit']])) {
    data <- model_fit(data, sim_args = sim_args)
  }
  
  if(!is.null(sim_args[['extract_coefficients']])) {
    data <- extract_coefficients(data)
  }

data
}
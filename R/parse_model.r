#' Parses tidy model simulation syntax
#' 
#' A function that parses the model simulation syntax in order to simulate data.
#' 
#' @param formula A named list with special model formula syntax. See details and examples
#'   for more information. The named list may contain the following:
#'   \itemize{
#'     \item fixed: This is the fixed portion of the model (i.e. covariates)
#'     \item random: This is the random portion of the model (i.e. random effects)
#'     \item error: This is the error (i.e. residual term).
#'   }
#' 
#' @export
#' @examples 
#' formula <- list(fixed = "resp_data ~ 4 * 1 + 0.5 * var1[mean = 2, sd = 5, var_type = 'single', dist_fun = 'rnorm'] + 0.75 * var2_f[numlevels = 2, var_type = 'single', replace = TRUE]", error = "~ 1[error_var = 1, err_gen = 'rnorm']")
#' 
parse_model_fixed <- function(formula) {
  
  
  
}
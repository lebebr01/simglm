#' Transform response variable
#' 
#' @param outcome The outcome variable to transform.
#' @param type Type of transformation to apply.
#' @param ... Additional arguments passed to distribution functions.
#' 
#' @export
transform_outcome <- function(outcome, type, ...) {
  
  if(type %in% c('logistic', 'binary')) {
    probability <- exp(outcome) / (1 + exp(outcome))
    rbinom(length(outcome), size = 1, 
           prob = probability)
  } else if(type %in% c('count', 'poisson')) {
    rpois(length(outcome), lambda = exp(outcome))
  } else {
    purrr::map()
  }
}


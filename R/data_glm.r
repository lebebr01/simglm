#' Transform response variable
#' 
#' @param outcome The outcome variable to transform.
#' @param type Type of transformation to apply.
#' @param ... Additional arguments passed to distribution functions.
#' 
#' @export
transform_outcome <- function(outcome, type, ...) {
  
  if(type %in% c('logistic', 'binary')) {
    probability <- convert_probability(outcome)
    rbinom(length(outcome), size = 1, 
           prob = probability)
  } else if(type %in% c('count', 'poisson')) {
    rpois(length(outcome), lambda = exp(outcome))
  } else if(type == 'multinomial') {
    
  } else if(type == 'ordinal') {
    probability <- do.call('cbind', 
          lapply(seq_along(1:ncol(outcome)), 
                 function(xx) convert_probability(outcome[, xx])
                 )
          )
    prob_categories <- probability_restructure(probability)
    do.call("rbind", lapply(seq_len(nrow(prob_categories)), 
      function(xx) 
        sample(x = 1:ncol(prob_categories), 
               size = 1,
               prob = prob_categories[xx, ]
        )
    ))
  } else {
    purrr::map()
  }
}
convert_probability <- function(logit) { 
  exp(logit) / (1 + exp(logit))
}
probability_restructure <- function(probability) {
  group_probs <- data.frame(matrix(nrow = nrow(probability), ncol = ncol(probability)+1))
  names(group_probs) <- paste0('prob', 1:(ncol(probability)+1))
  for(ii in seq_len(ncol(probability) + 1)) {
    if(ii == 1) {
      group_probs[, ii] <- 1 - probability[, ii]
    } else if(ii == ncol(group_probs)) {
      group_probs[, ii] <- probability[, ii - 1]
    } else {
      group_probs[, ii] <- probability[, ii - 1] - probability[, ii]
    }
  }
  group_probs
}

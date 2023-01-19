#' Transform response variable
#' 
#' @param outcome The outcome variable to transform.
#' @param type Type of transformation to apply.
#' @param categories A vector of named categories for multinomial sim
#' @param ... Additional arguments passed to distribution functions.
#' 
#' @export
transform_outcome <- function(outcome, type, categories = NULL, ...) {
  
  if(type %in% c('logistic', 'binary')) {
    probability <- convert_probability(outcome)
    rbinom(length(outcome), size = 1, 
           prob = probability)
  } else if(type %in% c('count', 'poisson')) {
    rpois(length(outcome), lambda = exp(outcome))
  } else if(type == 'multinomial') {
    probability <- cbind(1, do.call("cbind", lapply(seq_len(ncol(outcome)), 
                              function(xx) exp(outcome[, xx])
    )
    )
    )
    generate_multinomial_category(probability, categories)
  } else if(type == 'ordinal') {
    probability <- do.call('cbind', 
          lapply(seq_len(ncol(outcome)), 
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

generate_multinomial_category <- function(probability, categories = NULL) {
  outcome <- lapply(seq_len(nrow(probability)),
                    function(xx) 
                      rmultinom(n = 1, size = 1,
                                prob = probability[xx, ]))
  
  outcome_num <- do.call('c', lapply(outcome, detect_one))
  
  if(!is.null(categories)) { 
    outcome_category <- data.frame(outcome_num = rep(1:length(categories)), 
                                   outcome_categories = categories)
    outcome_comb <- merge(data.frame(ID = 1:length(outcome), 
                                    outcome_num = outcome_num),
                         outcome_category, by = 'outcome_num', all.x = TRUE)
    outcome_comb <- outcome_comb[order(outcome_comb$ID), ]
    outcome_comb <- subset(outcome_comb, select = -ID)
  } else {
      outcome_comb <- data.frame(outcome_num = outcome_num, 
                                 outcome_category = NA)
  }
  outcome_comb
}
detect_one <- function(outcome) {
  which(outcome == 1)
}

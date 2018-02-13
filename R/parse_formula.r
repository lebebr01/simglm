#' Parses tidy formula simulation syntax
#' 
#' A function that parses the formula simulation syntax in order to simulate data.
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
#' @examples 
#' 
#' 
parse_formula <- function(sim_args) {
  
  outcome <- sim_args$formula %>%
    as.character() %>%
    .[2]
  
  fixed <- sim_args$formula %>%
    as.character() %>%
    .[3] %>%
    gsub("\\+\\s*(\\s+|\\++)\\(.*?\\)", "", .) %>%
    gsub("^\\s+|\\s+$", "", .) %>%
    paste0("~", .) %>%
    as.formula()
  
  randomeffect <- sim_args$formula %>%
    as.character() %>%
    .[3] %>%
    regmatches(gregexpr("(\\+|\\s+)\\(.*?\\)", .)) %>%
    unlist() %>%
    gsub("^\\s+|\\s+$", "", .)
  
  list(outcome = outcome, 
       fixed = fixed,
       randomeffect = randomeffect)
}

#' Parses random effect specification
#' 
#' @param formula Random effect formula already parsed by \code{\link{parse_formula}}
#' 
#' @export 
parse_randomeffect <- function(formula) {
  
  cluster_id_vars <- lapply(seq_along(formula), function(xx) strsplit(formula, "\\|")[[xx]][2]) %>%
    unlist() %>%
    gsub("\\)", "", .) %>%
    gsub("^\\s+|\\s+$", "", .)
  
  random_effects <- lapply(seq_along(formula), function(xx) strsplit(formula, "\\|")[[xx]][1]) %>%
    unlist() %>%
    gsub("\\(", "", .) %>%
    gsub("^\\s+|\\s+$", "", .) %>%
    paste0('~', .)
  
  list(
    cluster_id_vars = cluster_id_vars,
    random_effects = random_effects
  )

}

#' Parse power specifications
#' 
#' @param sim_args A named list with special model formula syntax. See details and examples
#'   for more information. The named list may contain the following:
#'   \itemize{
#'     \item fixed: This is the fixed portion of the model (i.e. covariates)
#'     \item random: This is the random portion of the model (i.e. random effects)
#'     \item error: This is the error (i.e. residual term).
#'   }
#' @importFrom dplyr quo
#' @export 
parse_power <- function(sim_args) {
  
  if(is.null(sim_args$power$direction) || sim_args$power$direction %ni% c('lower', 'upper')) {
    number_tails <- 2
  } else {
    number_tails <- 1
  }
  
  if(is.null(sim_args$power$direction)) {
    sim_args$power$direction <- 'two-tailed'
  }
  
  if(is.null(sim_args$power$dist)) {
    sim_args$power$dist <- 'qnorm'
  }
  
  if(is.null(sim_args$power$alpha)) {
    sim_args$power$alpha <- 0.05
  }
  
  if(sim_args$power$direction == 'lower') {
    lower_tail <- TRUE
  } else {
    lower_tail <- FALSE
  }
  
  alpha <- sim_args$power$alpha / number_tails
  
  test_statistic <- purrr::invoke(sim_args$power$dist, 
                                  p = alpha, 
                                  lower.tail = lower_tail,
                                  sim_args$power$opts)

  list(test_statistic = test_statistic,
       alpha = sim_args$power$alpha, 
       number_tails = number_tails,
       direction = sim_args$power$direction,
       distribution = sim_args$power$dist
  )
  
}

#' Parse varying arguments
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
parse_varyarguments <- function(sim_args) {
  
  conditions <- expand.grid(sim_args[['vary_arguments']], KEEP.OUT.ATTRS = FALSE)
  if(any(sapply(conditions, is.list))) {
    loc <- sapply(conditions, is.list)
    simp_conditions <- conditions[loc != TRUE]
    list_conditions <- conditions[loc == TRUE]
    list_conditions <- lapply(seq_along(list_conditions), function(xx) 
      unlist(list_conditions[xx], recursive = FALSE))
    for(tt in seq_along(list_conditions)) {
      names(list_conditions[[tt]]) <- gsub("[0-9]*", "", names(list_conditions[[tt]]))
    }
    lapply(1:nrow(conditions), function(xx) c(sim_args, 
                                              simp_conditions[xx, , drop = FALSE], 
                                              do.call('c', lapply(seq_along(list_conditions), function(tt) 
                                                list_conditions[[tt]][xx]))
    ))
  } else {
    lapply(1:nrow(conditions), function(xx) c(sim_args, 
                                              conditions[xx, , drop = FALSE]))
  }
  
}


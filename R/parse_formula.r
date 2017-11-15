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

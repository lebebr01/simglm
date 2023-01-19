#' Simulate response variable
#' 
#' @param data Data simulated from other functions to pass to this function.
#' @param sim_args A named list with special model formula syntax. See details and examples
#'   for more information. The named list may contain the following:
#'   \itemize{
#'     \item fixed: This is the fixed portion of the model (i.e. covariates)
#'     \item random: This is the random portion of the model (i.e. random effects)
#'     \item error: This is the error (i.e. residual term).
#'   }
#' @param keep_intermediate TRUE/FALSE flag indicating whether intermediate steps
#'   should be kept. This would include fixed effects times regression weights,
#'   random effect summations, etc. Default is TRUE.
#' @param ... Other arguments to pass to error simulation functions.
#' 
#' @export 
generate_response <- function(data, sim_args, keep_intermediate = TRUE, ...) {
  
  outcome_name <- parse_formula(sim_args)[['outcome']]
  outcome_type <- sim_args[['outcome_type']]
  fixed_formula <- parse_formula(sim_args)[['fixed']]
  
  fixed_vars <- attr(terms(fixed_formula),"term.labels")
  
  if(any(grepl('^factor\\(', fixed_vars))) {
    fixed_vars <- gsub("factor\\(|\\)$", "", fixed_vars)
  }
  if(any(grepl('^ns\\(', fixed_vars))) {
    fixed_vars <- gsub("ns\\(|\\,.+\\)$", "", fixed_vars)
  }
  if(any(grepl("^poly\\(", fixed_vars))) {
    fixed_vars <- gsub("poly\\(|\\,.+\\)", "", fixed_vars)
  }
  
  if(any(grepl("^ns|^poly", attr(terms(fixed_formula), "term.labels")))) {
    fixed_vars <- poly_ns_names(sim_args)
  }
  
  if(any(unlist(lapply(seq_along(sim_args[['fixed']]), function(xx) 
    sim_args[['fixed']][[xx]]$var_type)) == 'factor')) {
    
    num_levels <- lapply(seq_along(sim_args[['fixed']]), function(xx) 
      sim_args[['fixed']][[xx]][['levels']])
    num_levels <- purrr::modify_if(num_levels, is.character, length)
    
    if(any(unlist(lapply(seq_along(sim_args[['fixed']]), function(xx) 
      num_levels[[xx]] > 1 & 
      sim_args[['fixed']][[xx]][['var_type']] == 'factor'))
    )) {
      fixed_vars <- factor_names(sim_args, fixed_vars)
    }
  }
  
  if(any(grepl(':', fixed_vars))) {
    fixed_vars <- gsub(":", "\\.", fixed_vars)
  }
  
  # Xmat <- model.matrix(fixed_formula, data.frame(data), contrasts.arg = contrasts)
  Xmat <- dplyr::select(data, dplyr::all_of(fixed_vars))
  if(any(grepl('Intercept', names(data)))) {
    Xmat <- cbind(data['X.Intercept.'], Xmat)
  }
  
  if(is.list(sim_args[['reg_weights']])) {
    fixed_outcome <- data.frame(do.call("cbind", 
              lapply(seq_along(sim_args[['reg_weights']]), 
                     function(xx) 
                       as.matrix(Xmat) %*% sim_args[['reg_weights']][[xx]])))
    names(fixed_outcome) <- paste0('logit', 1:ncol(fixed_outcome))
  } else {
    fixed_outcome <- as.matrix(Xmat) %*% sim_args[['reg_weights']]
  }
  
  if(length(parse_formula(sim_args)[['randomeffect']]) != 0) {
    random_formula <- parse_formula(sim_args)[['randomeffect']]
    random_formula_parsed <- parse_randomeffect(random_formula)
    random_effects_names <- names(sim_args[['randomeffect']])
    
    random_formula <- lapply(seq_along(random_formula_parsed[['random_effects']]), function(xx) 
      as.formula(random_formula_parsed[['random_effects']][xx]))
    
    Zmat <- lapply(lapply(random_formula, model.matrix, data = data), 
      data.frame)
    
    cross_class <- parse_crossclass(sim_args, parse_randomeffect(parse_formula(sim_args)[['randomeffect']]))
    if(any(cross_class[['cross_class_re']])){
      Zmat <- do.call('cbind', Zmat)
    } else {
      Zmat <- dplyr::bind_cols(Zmat)
    }
    
    rand_effects <- subset(data, select = random_effects_names)
    
    random_effects <- rowSums(rand_effects * Zmat)
  } else {
    random_effects <- NULL
    random_effects <- 0
  }
  
  if(keep_intermediate) {
    if(is.list(sim_args[['reg_weights']])) {
      response_outcomes <- data.frame(
        fixed_outcome,
        random_effects = random_effects
      )
    } else {
      response_outcomes <- data.frame(
        fixed_outcome = fixed_outcome,
        random_effects = random_effects
      )
    }
    
    data <- cbind(data, response_outcomes, row.names = NULL)
  }
  
  if(is.null(data[['error']])) {
    data['error'] <- 0
  }
  
  outcome <- fixed_outcome + random_effects + data[['error']]
  
  if(!is.null(sim_args[['outcome_type']])){
    if(is.null(sim_args[['multinomial_categories']])) {
      multinomial_categories <- NULL
    } else {
      multinomial_categories <- sim_args[['multinomial_categories']]
    }
    trans_outcome <- transform_outcome(outcome, 
                                       type = sim_args[['outcome_type']],
                                       categories = multinomial_categories)
    if(ncol(outcome) > 1) {
      names(outcome) <- paste0('untransformed_outcome', 1:ncol(outcome))
      data <- cbind(data, outcome) 
    } else {
      data <- cbind(data, untransformed_outcome = outcome)
    }
    if(sim_args[['outcome_type']] == 'multinomial') {
      data <- cbind(data, trans_outcome)
      if(is.null(multinomial_categories)) {
        names(data)[names(data) == 'outcome_num'] <- outcome_name
      } else {
        names(data)[names(data) == 'outcome_category'] <- outcome_name
      }
    } else {
      data[outcome_name] <- trans_outcome
    }
  } else {
    data[outcome_name] <- outcome
  }
  
  data
}

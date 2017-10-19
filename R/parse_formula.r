#' Parses tidy formula simulation syntax
#' 
#' A function that parses the formula simulation syntax in order to simulate data.
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
#' formula <- list(fixed = "resp_data ~ (4) * 1 + (0.5) * var1[mean = 2, sd = 5, var_type = 'single', dist_fun = 'rnorm'] + (0.75) * var2_f[numlevels = 2, var_type = 'single', replace = TRUE]", error = "~ 1[error_var = 1, err_gen = 'rnorm']")
#' parse_formula_fixed(formula)
#' 
parse_formula <- function(formula) {
  
  formula_elements <- names(formula)
  
  lapply(seq_along(formula_elements), function(xx) 
    tmp
    )
  
}


#' Parses fixed formula syntax
parse_formula_fixed <- function(formula) {
  
  fixed_formula <- formula$fixed
  
  fixed_input <- gsub("^\\s+|\\s+$", "", 
                      unlist(strsplit(fixed_formula, split = '~'))[2])
  
  fixed_parameters <- unlist(regmatches(fixed_input, 
                                        gregexpr("\\[(.*?)\\]", fixed_input)))
  
  fixed_input_noparam <- gsub("\\[(.*?)\\]", "", fixed_input)
  
  fixed_beta <- unlist(regmatches(fixed_input, 
                                  gregexpr("\\([0-9]*\\.?[0-9]*\\)", fixed_input)))
  fixed_beta <- as.numeric(gsub("\\(|\\)", "", fixed_beta))
  
  fixed_formula <- gsub("\\[(.*?)\\]|\\([0-9]*\\.?[0-9]*\\)|\\*", "", fixed_input)
  fixed_formula <- as.formula(paste0("~ ", gsub("^\\s+|\\s+$", "", fixed_formula)))
  
  list(fixed_formula = fixed_formula, 
       fixed_parameters = fixed_parameters, 
       fixed_beta = fixed_beta)
  
}

#' Parses outcome from formula syntax
parse_formula_outcome <- function(formula) {
  
  gsub("^\\s+|\\s+$", "", 
       unlist(strsplit(formula$formula, split = '~'))[1])
  
}

#' Parses error formula syntax
parse_formula_error <- function(formula) {
  
  error_formula <- formula$error
  
  error_parameters <- unlist(regmatches(error_formula, 
                                        gregexpr("\\[(.*?)\\]", error_formula)))
  
}

#' Parses type of variable generation fixed effects
parse_fixed_type <- function(fixed_parameters) {
  
  if(grepl("dist_fun", fixed_parameters)) {
    'sim_continuous'
  } else {
    if(grepl('numlevels', fixed_parameters)) {
      'sim_factor'
    } else {
      'sim_knot'
    }
  }
  
}


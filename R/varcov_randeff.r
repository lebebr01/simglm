#' Function to create random effect variance-covariance matrices
#' 
#' Input variances of random effects and correlation between random effects,
#' returns variance-covariance matrix of random effects.
#' 
#' @param random_var Variance of random effects.
#' @param cor_re Correlation between random effects, currently only a 
#'   constant supported.
varcov_randeff <- function(random_var, cor_re) {
  
  if (any(random_var < 0)) 
    stop("All 'random_var' should be positive")
  if(cor_re == 0 | length(random_var) == 1) {
    mat <- diag(random_var, nrow = length(random_var), 
                ncol = length(random_var))
  } else {
    mat <- diag(1, nrow = length(random_var), ncol = length(random_var))
    delta <- row(mat) - col(mat)
    mat[delta != 0] <- cor_re
    
    sd <- sqrt(random_var)
    n <- sqrt(length(mat))
    if (n != length(sd)) 
      stop("The length of 'random_var' should be the same as the number 
           of rows of 'mat'")
    mat <- diag(sd) %*% mat %*% diag(sd)  
  }
 mat
}

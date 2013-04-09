#' Function to create random effect variance-covariance matrices
#' 
#' Input variances of random effects and correlation between random effects,
#' returns variance-covariance matrix of random effects.
#' 
#' @param random.param Variance of random effects.
#' @param cor Correlation between random effects, currently only a constant supported.
#' 
#' @export 
varcov.randeff <- function(random.param, cor){
  
  if (any(random.param < 0)) 
    stop("All 'random.param' should be positive")
  if(cor == 0 | length(random.param == 1)) {
    mat <- diag(random.param, nrow = length(random.param), ncol = length(random.param))
  } else {
    mat <- diag(1, nrow = length(random.param), ncol = length(random.param))
    delta <- row(mat) - col(mat)
    mat[delta != 0] <- cor
    
    sd <- sqrt(random.param)
    n <- sqrt(length(mat))
    if (n != length(sd)) 
      stop("The length of 'random.param' should be the same as the number of rows of 'mat'")
    mat <- diag(sd) %*% mat %*% diag(sd)  
  }
 return(mat)
}
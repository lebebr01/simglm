#' Function to correlate variables
#' 
#' Inputs a matrix and other parameters and outputs a correlated matrix
#' 
#' @param mat A matrix of variables to correlate
#' @param cor_vars A vector of correlations to specify, must be specified by row
#'    where the first element is the correlation between variable 1 and variable 2, 
#'    second correlation is between variable 1 and variable 3, and so on.
#' @param cov_param Variable specification similar to specifying fixed effects. 
#'    See \code{\link{sim_reg}} for more details.
#' @param standardize TRUE/FALSE flag indicating whether variables should be standardized
#'    prior to correlating (this is needed for accurate correlated variables)
#' @export
#' 
corr_variables <- function(mat, cor_vars, cov_param, standardize = TRUE) {
  
  if(standardize) {
    cov_data <- purrr::invoke_map(cov_param$dist_fun, cov_param$opts, 
                                  n = 1000000)
    cov_mu <- round(sapply(cov_data, mean), 2)
    cov_sd <- round(sapply(cov_data, sd), 2)
  } else {
    cov_mu <- 0
    cov_sd <- 1
  }
  
  Xmat <- do.call('cbind', lapply(seq_len(ncol(mat)), function(xx) 
    standardize(mat[, xx], mean = cov_mu[xx], sd = cov_sd[xx])))
  
  c_mat <- matrix(nrow = ncol(mat), ncol = ncol(mat))
  diag(c_mat) <- 1
  c_mat[upper.tri(c_mat)] <- c_mat[lower.tri(c_mat)] <- cor_vars
  cov <- diag(cov_sd) %*% c_mat %*% diag(cov_sd)
  es <- eigen(cov, symmetric = TRUE)
  ev <- es$values
  Xmat <- t(cov_mu + es$vectors %*% diag(sqrt(pmax(ev, 0)), 
                                         length(cov_sd)) %*% t(Xmat))
  
  Xmat
}

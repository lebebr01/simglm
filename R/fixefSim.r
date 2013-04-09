#' Simulates design matrix
#' 
#' Input fixed variables, sample size, and number of within variables, returns design matrix.
#' 
#' @param fixed.vars Character vector of covariates for design matrix.
#' @param n Number of clusters.
#' @param p Number of within cluster units.
#' @param w.var Number of time varying covariates.
#' @export 
fixef.sim <- function(fixed.vars, n, p, w.var){
  
  n.vars <- length(fixed.vars)
  n.int <- length(grep(":",fixed.vars))
  Xmat <- matrix(nrow=n*p,ncol = ifelse(w.var == 1, 1, 2))
  
  if(w.var == 1){
    Xmat[,1] <- rep.int(1,times = n*p)
  } else {
    if(w.var == 2){
      Xmat[,1] <- rep.int(1, times = n*p)
      Xmat[,2] <- rep.int((1:p)-1,times = n)
    } else { 
      Xmat[,1] <- rep.int(1, times = n*p)
      Xmat[,2] <- rep.int((1:p)-1,times = n)
      Xmat <- cbind(Xmat, do.call("cbind", lapply(3:w.var, function(xx) 
        rnorm(n * p, mean = cov.param[[xx-2]][1], sd = cov.param[[xx-2]][2]))))
    }
  }
  
  if(n.int == 0){
    if(w.var != n.vars+1){
      Xmat <- cbind(Xmat, do.call("cbind", lapply((w.var+1):(n.vars+1), function(xx)
        rep(rnorm(n, mean = cov.param[[xx-2]][1], sd=cov.param[[xx-2]][2]), each = p))))
  } else {
    next;
  }
} else {
  num.no.int <- n.vars - n.int                  
  if(w.var != num.no.int+1){
    Xmat <- cbind(Xmat, do.call("cbind", lapply((w.var+1):(num.no.int+1), function(xx)
      rep(rnorm(n, mean = cov.param[[xx-2]][1], sd=cov.param[[xx-2]][2]), each = p))))
} else {
  next;
}
}

if(n.int == 0){
  colnames(Xmat) <- c("Int", fixed.vars)
} else {
  int.loc <- grep(":", fixed.vars)
  colnames(Xmat) <- c("Int",fixed.vars[-int.loc])
}
Xmat <- model.matrix(fixed, data.frame(Xmat))
Xmat
}

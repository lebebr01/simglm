#' Function to simulate random effects.
#' 
#' Input simulation parameters and returns random effects.
#' 
#' @param random.param Variance of random effects. Must be same length as random.
#' @param Cor Correlation between random effects.
#' @param n Cluster sample size.
#' @param dist Simulated random effect distribution.  Must be "lap", "chi", "norm", "bimod", 
#' "norm" is default.
#' @parm num.dist Number of distributions for bimod random variables
#' @import MASS VGAM
#' @export 
rand.eff.sim <- function(random.param, cor, n, dist = c("lap","chi","norm", "bimod"), num.dist){

  if(dist == "lap"){ 

    reff <- do.call("cbind", lapply(1:length(random.param), function(xx) rlaplace(n, 0, 1)))
    c <- varcov.randeff(random.param, cor)
    reff1 <- reff %*% chol(c/2)

  } else {
    if(dist == "chi"){ 
      
      reff <- do.call("cbind", lapply(1:length(random.param), function(xx) rchisq(n, 1)))
      reff <- reff-1
      c <- varcov.randeff(random.param, cor)
      reff1 <- reff %*% chol(c/2)

    } else {
      if(dist == "bimod"){
        
        reff <- do.call("cbind", lapply(1:length(random.param), function(xx) 
          rbimod(n, mean = rep(0, num.dist), var = rep(1, num.dist), num.dist)))
        c <- varcov.randeff(random.param, cor)
        reff1 <- reff %*% chol(c/2)

      } else { 
        #reff <- do.call("cbind", lapply(1:length(random.param), function(xx) 
         # rnorm(n, mean = 0, sd = sqrt(random.param[xx]))))
         c <- varcov.randeff(random.param, cor)
        #reff1 <- reff %*% chol(sqrt(c)/2)
         reff1 <- mvrnorm(n, rep.int(0, length(random.param)), c)
                  
    }
   }
  }
 return(reff1)  
}
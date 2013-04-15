#' Function to extract coefficient table from lmer object
#' 
#' Input lmer object output coefficient table
#' 
#' Extracts coefficients from lmer object for use to check adequacy of simulation.
#'
#' @param fm lmer object 
#'  
coef.tbl <- function(fm)
{
  ## check that fm is an object of the "mer" class
  stopifnot(is(fm, "mer"))
  cc <- fixef(fm)
  ss <- sqrt(diag(vcov(fm)))
  data.frame(Estimate = cc, Std.Err = ss, t = cc/ss, row.names = names(cc))
  
}
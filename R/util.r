"%ni%" <- Negate("%in%")

prop_limits <- function(prop) {
  if(prop > .5) {
    u_diff <- 1 - prop
    u_max <- 1
    u_min <- prop - u_diff
  } else {
    u_diff <- 0 + prop
    u_max <- prop + u_diff
    u_min <- 0
  }
  return(c(u_min, u_max))
}

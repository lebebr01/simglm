testingVar <- function(n, p, num.dist, errorVar){
  err <- unlist(lapply(1:n, function(x){
    ((rbimod(p, mean = rep(0, num.dist), var = rep(1, num.dist), num.dist)
      *chol((errorVar)))) }))
  var(err)
}

testingVarChi <- function(n, p, errorVar){
  err <- unlist(lapply(1:n, function(x){((rchisq(p,1)-1)/sqrt(2))*sqrt(errorVar)}))
  var(err)
}

testingVarLap <- function(n, p, errorVar){
  err <- unlist(lapply(1:n, function(x){rlaplace(p,0,1)*chol((errorVar/2))}))
  var(err)
}
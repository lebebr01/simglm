##Function to generate block diagonal matrix
blockDiagonal <- function(X, nrow, ncol) {
  dimRow <- dim(X)[1]
  dimCol <- dim(X)[2]
  bDiagMat <- matrix(0, nrow=nrow, ncol=ncol)
  matRows <- c((1:n)+((dimRow-1)*(1:n-1)),nrow+1)
  matCols <- c((1:n)+((dimCol-1)*(1:n-1)),ncol+1)
  for(i in 1:n){
    bDiagMat[matRows[i]:(matRows[i+1]-1),matCols[i]:(matCols[i+1]-1)] <- X
  }
  bDiagMat
}

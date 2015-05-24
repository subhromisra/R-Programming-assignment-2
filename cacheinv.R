cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  if(nrow(data) == ncol(data)){
    if(det(data) == 0){
      message("Non Invertible Matrix")
      return(NA)
    }
    else{
      m <- solve(data, LINPACK = TRUE, ...)
      x$setinv(m)
      m
    }
  }
  else{
    message("Not a square matrix")
    return(NA)
  }
}

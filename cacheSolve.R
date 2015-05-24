cacheSolve <- function(X, ...) {
  Xinv <- X$getinverse()
  if(!is.null(Xinv)) {
    message("getting cached data")
    return(Xinv)
  }
  data <- X$get()
  Xinv <- solve(data)
  X$setinverse(Xinv)
  Xinv
}
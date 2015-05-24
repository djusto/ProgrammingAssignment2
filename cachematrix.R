## The function cacheSolve computes the inverse of matrix X.
##   If it is already computed, then return the cached data.
##   It uses the makeCacheMatrix "matrix" object that can
##   cache its inverse.

## makeCacheMatrix function is a special "matrix" object.
##   set:  store the data matrix.
##   get:  return the data matrix.
##   setinverse: store the inverse of the matrix
##   getinverse: return the inverse of the matrix


makeCacheMatrix <- function(X = matrix()) {
    invX <- NULL
    set  <- function(Y) {
        X    <<- Y                        # set X    
        invX <<- NULL
    }
    get        <- function() X           # return X
    setinverse <- function(inverseX)
                  invX <<- inverseX
    getinverse <- function() invX

    list(set        = set,
         get        = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Return the inverse of 'X' if it is already computed, otherwise
##   computes the inverse using 'solve', stores the inverse and
##   return the inverse matrix. 
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

makeCacheMatrix <- function(X = numeric()) {
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
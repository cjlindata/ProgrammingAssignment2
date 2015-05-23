##
## The function creates a special "matrix" object that can cache its inverse
## 
makeCacheMatrix <- function(x = matrix()) {
        ## Set Initial InvM value to NULL
        invM <- NULL  
        ## Set a new matrix value and clean up old InvM value vif any
        set <- function(y){
            x <<- y
            invM <<- NULL
        }
        ## get Original Matrix
        get <- function() x
        ## build-in solve(A) where A is a square Matrix
        setInverse <- function(inverseM) invM <<- inverseM
        getInverse <- function() invM
        
        ## Set up functions list
        list(set= set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## 
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. If the
## inverse has already been calculated (and the matrix has not changed), then the cachesolve should
## retrieve the inverse from the cache
##
cacheSolve <- function(x, ...) {
        invM <- x$getInverse()
        
    
        if(!is.null(invM)){
            message("Getting cached data")
            return (invM)
        }
        ## set new matrix and inverse matrix
        data <- x$get()
        invM <- solve(data)
        x$setInverse(invM)
        invM    
}

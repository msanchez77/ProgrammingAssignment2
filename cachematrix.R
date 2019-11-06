## Put comments here that give an overall description of what your
## functions do

## Initializes the special matrix that contains getters and setters for
## the matrix itself, and the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    x <- NULL
    set <- function(y) {
         x <<- y
         i <<- NULL
    }
    
    get <- function() x
    setInverse <- function(solve) i <<- solve
    getInverse <- function() i
    matrix(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## Retrieves the inverse of x. If it is not already cached, it will 
## compute it here, store it in cache, then return it. If it is cached, it
## will simply return it

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    i <-x$getInverse()
    if (!is.null(i)) {
        message("Inverse already in cache, returning cached value")
        return(i)
    }
    
    ## Inverse NOT in cache
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}

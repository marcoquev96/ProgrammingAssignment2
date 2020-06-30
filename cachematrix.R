## Put comments here that give an overall description of what your
## functions do

## This function takes a matrix 'x' and makes a special matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m = NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of a matrix 'x'; however, it first checks to see if it
## hasn't been computed yet and the matrix 'x' hasn't changed, then this function retrieves
## the inverse from the cache

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if (!is.null(m)) {
        message("Getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
        ## Return a matrix that is the inverse of 'x'
}

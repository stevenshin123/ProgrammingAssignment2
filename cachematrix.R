## cachematrix.R
##
## Matrix Inversion is usually a costly computation and caching the inverse
## may provide some benefit rather than computing it repeatedly.
## makeCacheMatrix will create a special "matrix" object that caches its
## inverse. Then, cacheSolve will compute and cache the inverse of the special 
## "matrix". If the inverse has already been calculated (and the matrix has not
## changed, then, cacheSolve should retrieve the inverse from the cache.
## For this assignment, ASSUME MATRIX IS ALWAYS INVERTIBLE


## This function creates a special "matrix" object that can cache its inverse.
## cim = Cached Inverse Matrix

makeCacheMatrix <- function(x = matrix()) {
    cim <- NULL
    set <- function(y) {
        x <<- y
        cim <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) cim <<- inverse
    getinv <- function() cim
    matrix(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## This function computes the inverse of the special "matrix" retured by
## makeCacheMatrix above. 
## cim = Cached Inverse Matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    cim <- x$getinv()
    if(!is.null(cim)) {
        message("getting cached data")
        return(cim)
    }
    data <- x$get()
    cim <- solve(data, ...)
    x$setinv(cim)
    cim
}

## The following set of functions allows to calculate and cache the
## value of the inverse of a matrix so that when needed again, it can
## be looked up in the cache instead of re-calculating it.

## Author: Pedro Ramirez (PowerChile)
## Date: February 13, 2015
## London, UK


## The function 'makeCacheMatrix' receives as argument an object of
## class matrix and creates a special "matrix" object, which is a
## list containing a function to:
##   1. set the value of the matrix ('set')
##   2. get the value of the matrix ('get')
##   3. set the value of the matrix's inverse ('setinv')
##   4. get the value of the matrix's inverse ('getinv')

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x   <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## The function 'cacheSolve' receives as argument an object of class
## list, i.e. the special "matrix" created with the function
## 'makeCacheMatrix', and calculates its inverse. Before calculating
## the matrix's inverse, this function first checks if the inverse
## has already been calculated, and if so, it 'get's the inverse from
## the cache and skips the computation. If the inverse has not been
## calculated, on the other hand, this function calculates the inverse
## of the data and sets the value of the inverse in the cache via the
## 'setinv' function.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}

## end-of-file
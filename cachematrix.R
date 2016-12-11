## This R file caches the inverse of a matrix
## Two functions are defined in this file:
## (1) makeCacheMatrix: this function creates a matrix object to cache the inverse.
## (2) cacheSolve: this function computes the inverse of the matrix returned by the makeCacheMatrix in function (1).


# (1) This function creates a matrix object to cache the inverse.
makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     set <- function(z) {
          x <<- z
          i <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) i <<- inverse
     getinverse <- function() i
     list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# (2) This function computes the inverse of the matrix returned by the makeCacheMatrix in function (1).
# If the inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
     i <- x$getinverse()
     if(!is.null(i)) {
          message("Use data from cache")
          return(i)
     }
     data <- x$get()
     i <- solve(data, ...)
     x$setinverse(i)
     i
}

## Put comments here that give an overall description of what your
## functions do

## This set of functions creates a matrix object, computes the inverted matrix, and caches the inverted matrix for future retrieval without the need to recompute the inverted matrix.

## Write a short comment describing this function
## makeCacheMatrix creates the matrix object and sets, gets, and caches, the value of the inverted matrix.

makeCacheMatrix <- function(x = matrix()) {

     invm <- NULL
     set <- function(y) {
          x <<- y
          invm <<- NULL
     }
     # get the original matrix
     get <- function() { x }
     # set the the inverted matrix
     setinv <- function(nv) { invm <<- nv }
     # get the inverted matrix
     getinv <- function() { invm }
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)

}


## Write a short comment describing this function
## cacheSolve computes the inverted matrix for the original matrix and retrieves
## the value of the the inverted matrix. The cached inverted matrix is retrieved if inverted matrix has already be computed.

cacheSolve <- function(x, ...) {

     ## Return a matrix that is the inverse of 'x'
     invm <- x$getinv()
     # get the cached value of the inverted matrix if it exists
     if(!is.null(invm)) {
          message("getting cached data")
          return(invm)
     }
     # get the original matrix
     data <- x$get()
     # compute the inverse of the original matrix
     invm <- solve(data, ...)
     # set the value of the inverted matrix
     x$setinv(invm)
     # return the inverted matrix
     invm

}

## The following functions makeCacheMatrix and cacheSolve are used to compute
## and cache the inverse of a matrix. If the inverse is computed it is retrieved
## from cache rather than to compute it repeatedly.  

## The function makeCacheMatrix creates a matrix object which can cache
## its inverse. It contains a list which can
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(a) inverse <<- a
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated (and the matrix
## has not changed), then the cachesolve retrieves the inverse from the cache. 
## If not already calculated, it computes and sets the inverse. 

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}

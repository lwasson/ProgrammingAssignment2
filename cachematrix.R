## The following functions (1) cache the inverse of a matrix and then
## (2) retrieve the inverse from the cache, if it exists.  Otherwise,
## the inverse is calculated.

## makecacheMatrix is a function that caches the inverse of the matrix,
## so that it does not need to be recalculated

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
        x <<- y
        inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv) 
}


## This function returns the inverse of the input 
## to the makecachematrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv = x$getinv()
  ## check to see if the inverse has been calculated
  if(!is.null(inv)) {
    message("Retrieving the cached data")
    return(inv)
  }
  ## otherwise calculate the inverse
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
  
}

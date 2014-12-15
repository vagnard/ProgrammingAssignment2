## Input: invertible matrix, Output: list of 4 functions on matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(y) inv <<- y
  getinv <- function() inv
  list(set = set, get = get, getinv = getinv, setinv = setinv)
}


## Input: list of 4 functions from function makeCacheMatrix
## Output: inverse of a matrix, defined by makeCacheMatrix

cacheSolve <- function(x, ...) {
  inv = x$getinv()
  if (!is.null(inv)) 
    return (inv)
  inv <- solve(x$get(), ...)
  x$setinv(inv)
  inv
}
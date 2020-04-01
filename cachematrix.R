## Daniel Orozco - Coursera - Programming Assigment 2
## Functions to cache the inverse of a Matrix, to save potentially time-consuming computation

## Description: Function to cache a matrix
## Arguments: Matrix
## Return value: List (Special "matrix" with the cache)
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Description: Function to calculate the inverse of a matrix if it's not already in cache
## Arguments: Matrix
## Return value: Matrix (Inverse)
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}

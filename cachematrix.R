## The two functions below are designed to create a special matrix and cache its inverse

## This is the function that creates this special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solveMatrix) i <<- solveMatrix
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the matrix returned by the above function.
## If the matrix has not changed, the inverse of it that has been calculated before is retrieved from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

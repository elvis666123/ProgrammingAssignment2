## Create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Compute the inverse of the matrix returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}
The makeCacheMatrix function takes a matrix x as input (or creates an empty matrix if none is given) and returns a list of four functions:

set: sets the value of the matrix
get: gets the value of the matrix
setinverse: sets the inverse of the matrix (for caching purposes)
getinverse: gets the inverse of the matrix (for caching purposes)
The cacheSolve function takes a makeCacheMatrix object x as input and computes the inverse of the matrix using the solve function. If the inverse has already been computed and cached in the makeCacheMatrix object, it retrieves it from the cache instead of recomputing it. If the inverse has not been cached, it computes the inverse, caches it in the makeCacheMatrix object, and returns the inverse.







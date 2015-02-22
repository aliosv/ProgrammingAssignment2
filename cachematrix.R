## Below are two functions that are used to create a special object that stores a matrix and cache's its inverse.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  get <- function() x
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  getInverse <- function() inverse
  setInverse <- function(y) inverse <<- y
  
  list(get = get, 
       set = set,
       getInverse = getInverse,
       setInverse = setInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  
  if(!is.null(inverse)) {
    # message for testing
    message("getting cached data")
    
    return(inverse)
  }
  
  inverse <- solve(x$get(), ...)
  
  x$setInverse(inverse)
  
  inverse
}

# Example:

# x <- makeCacheMatrix(matrix(c(1,3,1,4), 2, 2))
# x$getInverse()
# cacheSolve(x)
# cacheSolve(x)
# x$set(matrix(c(2,3,1,5), 2, 2))
# cacheSolve(x)
# cacheSolve(x)
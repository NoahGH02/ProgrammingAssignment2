## These functions are used to create a special matrix object that can cache its inverse.
## This caching mechanism helps to optimize the performance of repeated matrix inversion
## computations by storing the inverse of the matrix after the first computation and 
## retrieving it from the cache for subsequent requests, avoiding redundant calculations.

# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL  # Initialize the inverse property to NULL
  
  # This function sets the matrix and resets the cached inverse to NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # This function returns the matrix
  get <- function() x
  
  # This function sets the inverse of the matrix
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  
  # This function returns the inverse of the matrix
  getInverse <- function() inv
  
  # Return a list of the above functions, allowing access to them
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed),
# then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  inv <- x$getInverse()  # Get the cached inverse
  
  # If the inverse is already cached, return it
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Otherwise, calculate the inverse
  data <- x$get()  # Get the matrix
  inv <- solve(data, ...)  # Compute the inverse
  
  x$setInverse(inv)  # Cache the inverse
  
  inv  # Return the inverse
}

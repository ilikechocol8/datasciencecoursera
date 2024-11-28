
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL  # Initialize the cache for the inverse
  
  set <- function(y) {
    x <<- y       # Set the matrix
    inv <<- NULL  # Reset the inverse cache when matrix is updated
  }
  
  # Get the matrix value
  get <- function() x
  
  # Set the cached inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # Get the cached inverse
  getInverse <- function() inv
  
  # Return a list of functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # Retrieve the cached inverse from the object
  inv <- x$getInverse()
  
  # Check if the inverse is already cached
  if(!is.null(inv)) {
    message("Getting cached data")
    return(inv)  # Return cached inverse
  }
  
  # Get the matrix from the object
  data <- x$get()
  
  # Calculate the inverse of the matrix
  inv <- solve(data, ...)
  
  # Cache the inverse for future use
  x$setInverse(inv)
  
  # Return the calculated inverse
  inv
}

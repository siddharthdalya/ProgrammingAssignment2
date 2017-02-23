# This function creates a list, which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse of matrix
# get the value of the inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
  #Variable to hold value of inverse of a matrix
  inverse <- NULL
  # Define a set function to set matrix value
  set <- function(mat) {
    x <<- mat
    # Reset value of inverse to NULL.
    inverse <<- NULL
  }
  # Return the value of matrix.
  get <- function() x
  # Set the value of inverse of a matrix.
  setinverse <- function(inverseMatrix) inverse <<- inverseMatrix
  # Return the value of inverse of a matrix.
  getinverse <- function() inverse
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then cacheSolve retrieves
# the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  # Check if matrix inverse is cached.
  if (!is.null(inv)) {
    message('Inverse retrieved from cache')
    # Return cached matrix inverse.
    return(inv)
  }
  # Get the matrix.
  mat <- x$get()
  # Calculate the inverse of matrix.
  inv <- solve(mat)
  # Cache the value of inverse of matrix.
  x$setinverse(inv)
  # Return matrix inverse.
  inv
}

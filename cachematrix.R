# Programming Assignment 2: Caching the Inverse of a Matrix
# This file contains functions for calculating and caching
# the inverse of a matrix using lexical scoping.


# makeCacheMatrix:
# Creates a special matrix object that can cache its inverse.
# The function provides methods to set and retrieve the matrix,
# as well as store and retrieve its calculated inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL

  # Update the matrix and clear any previously cached inverse.
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }

  # Return the current matrix.
  get <- function() {
    x
  }

  # Store the calculated inverse in the cache.
  setinverse <- function(solve) {
    inverse <<- solve
  }

  # Return the cached inverse.
  getinverse <- function() {
    inverse
  }

  # Return a list of functions for managing the matrix and its inverse.
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


# cacheSolve:
# Calculates the inverse of a matrix using solve().
# If the inverse has already been calculated and cached,
# the function retrieves it instead of performing the calculation again.
# The newly calculated inverse is cached for future use.

cacheSolve <- function(x, ...) {

  # Check whether an inverse is already stored in the cache.
  inverse <- x$getinverse()

  # Return the cached inverse if it exists.
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }

  # Retrieve the matrix and calculate its inverse.
  data <- x$get()
  inverse <- solve(data, ...)

  # Store the calculated inverse for future use.
  x$setinverse(inverse)

  # Return the calculated inverse.
  inverse
}

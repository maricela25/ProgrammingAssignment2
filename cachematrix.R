## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This script contains two functions to efficiently compute and cache
## the inverse of a matrix. This can save computational time when dealing
## with large matrices that are repeatedly inverted.

## The function makeCacheMatrix creates a special object that stores
## a matrix and can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # Initialize the inverse as NULL
  
  # Function to set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL # Reset the cached inverse
  }
  
  # Function to get the value of the matrix
  get <- function() x
  
  # Function to set the value of the cached inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # Function to get the cached inverse
  getInverse <- function() inv
  
  # Return a list of the above functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
## Write a short comment describing this function

## The function cacheSolve computes the inverse of the "matrix" created
## by makeCacheMatrix. If the inverse has already been computed and the
## matrix has not changed, cacheSolve retrieves the cached inverse.
cacheSolve <- function(x, ...) {
  # Check if the inverse is already cached
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data") # Message to indicate cached result
    return(inv) # Return the cached inverse
  }
  
  # If not cached, compute the inverse
  data <- x$get() # Get the matrix
  inv <- solve(data, ...) # Compute the inverse using solve()
  x$setInverse(inv) # Cache the inverse
  inv # Return the computed inverse
}


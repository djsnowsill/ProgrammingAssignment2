## File: cacheMatrix.R
## Author: D Snowsill
## Description: This file contains to functions which when used together provide caching for potentially expensive
## matrix inversion operations. The first function, makeCacheMatrix, returns a wrapper object around the passed in matrix which
## provides four functions to manipulate the passed in matrix and the caching of the inversion of the matrix
## The second function, cacheSolve, assumes a vector which was created by the makeCacheMatrix is passed in and used the cached matrix
## if available otherwise it solves for the inverse matrix, caches the matrix and returns the result


## Function Name: makeCacheMatrix
## Arguments: x needs to be an inversible matrix 
## Description: This function creates a wrapper object around the passed in matrix and provides functions to cache a matrix
## Example: mcm <- makeCacheMatrix( rbind(c(1, -1/4), c(-1/4, 1))  )
makeCacheMatrix <- function(x = matrix()) {
  
  ## Reset cache contents
  i <- NULL
  
  ## Function to set matrix which will be inverted by cacheSolve function
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## Returns matrix which was previously set
  get <- function() x
  
  ## Caches the passed in matrix
  setcachedmatrix <- function(inverse) i <<- inverse
  
  ## Returns the cached matrix
  getcachedmatrix <- function() i
  
  ## As last line in function, it returns a vector of functions which can be used elsewhere
  list(set = set, get = get, setcachedmatrix = setcachedmatrix, getcachedmatrix = getcachedmatrix)
}


## Function Name: cacheSolve
## Arguments: x which is a vector of functions returned from makeCacheMatrix, ... any optional arguments to be passed to solve function
## Description: Return a matrix that is the inverse of 'x' using a cached inverse matrix if its available
## Example: cacheSolve(mcm) where mcm <- makeCacheMatrix( rbind(c(1, -1/4), c(-1/4, 1))  )
cacheSolve <- function(x, ...) {

  ## Lookup cached matrix
  i <- x$getcachedmatrix()
  
  ## If cached matrix is available (i.e. not null) return cached matrix
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## As cached matrix is not available, calculate inverse matrix and cache matrix
  data <- x$get()
  i <- solve(data, ...)
  x$setcachedmatrix(i)
  
  ## Return inverse
  i
}

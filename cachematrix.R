## These functions attempt to cache the inverse of a matrix in order to 
## only run a matrix inversion once.

## The makeCacheMatrix creates the special matrix object that can cache 
## its inverse.  

makeCacheMatrix <- function(x = matrix()) {
      # Set the inverse to null
      s <- NULL
      # This initialises the special matrix object
      # Notice the inverse, s, is set to NULL:
      set <- function(y) {
        x <<- y
        s <<- NULL
      }
      #  This function allows cacheSolve to check whether
      #  the matrix has already been solved
      get <- function() x
      setsolve <- function(solve) s <<- solve
      getsolve <- function() s
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}


## This function computes the inverse of the matrix object returned by
## the function makeCacheMatrix, if the inverse has already been calculated
## then cacheSolve retrieves the inverse from cache.
## The computation uses the "solve" function in R, which computes the inverse
## of a square matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        # This section checks whether the matrix has been solved
        if(!is.null(s)) {
          # Matrix has already been "solved"
          # get the cached result:
          message("getting cached inverse")
          return(s)
        }
        # Matrix has not been solved
        # Get the matrix:
        data <- x$get()
        # Calculate the inverse
        s <- solve(data, ...)
        # This sets the value of s to the inverse
        # thereby caching the value
        x$setsolve(s)
        # This returns the inverse
        s
}

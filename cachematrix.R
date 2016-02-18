## These functions take a matrix as input and calculate its inverse.
## The result is cached, so it can be recalled without using the system resources
##    to calculate the inverse again.

## The first function creates a 'matrix' of functions, 
##    which are output using the list function.

makeCacheMatrix <- function(x = matrix()) {
      s <- NULL
      set <- function(y) {
            x <<- y
            s <<- NULL
      }
      get <- function() x
      setsolve <- function(solve) s <<- solve
      getsolve <- function() s
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
      
}


## This second function takes the output of makeCacheMatrix, and
##     checks if the solution already exists in the cache
##     if it does, then it returns the message 'getting cached data' and prints the solution.
##     if not, then it calculates the inverse, sets the value in cache, and prints the solution.

cachesolve <- function(x, ...) {

      s <- x$getsolve()
      if(!is.null(s)) {
            message("getting cached data")
            return(s)
      }
      data <- x$get()
      s <- solve(data, ...)
      x$setsolve(s)
      s
      ## Returns a matrix that is the inverse of 'x'     
}

## Functions that allow caching the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  # Creates a special "matrix" object that can cache its inverse.
  #
  # Args:
  #   x: a square numeric invertable matrix.
  #
  # Returns:
  #   The list of functions to set/get matrix and its inverse.
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) i <<- solve
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
  # This function computes the inverse of the special "matrix" returned 
  # by makeCacheMatrix above. If the inverse has already been calculated 
  # (and the matrix has not changed), then the cacheSolve should retrieve 
  # the inverse from the cache.
  #
  # Args:
  #   x: a square numeric invertable matrix.
  #   ...: further arguments passed to or from other methods
  #
  # Returns:
  #   The matrix that is the inverse of 'x'
  startTime <- Sys.time()
  i <- x$getInverse()
  if(!is.null(i)) {
    message("Getting cached data")
    endTime <- Sys.time()
    duration <- endTime - startTime
    message("Elapsed time: ", duration)
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  endTime <- Sys.time()
  duration <- endTime - startTime
  message("Elapsed time: ", duration)
  i
}

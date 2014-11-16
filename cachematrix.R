## These two functions allow to create a matrix with a cache for the inverse matrix
## and the functionality to get the inverse. Either through calculating the inverse
## and storing the result in the cahce or by using the cached data.

## Create a special matrix that contains functions to cache the inverse of this matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  ## Set the matrix to the given value and reset the inverse
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  ## Get the matrix
  get <- function() x
  
  ## Set the inverse of the matrix by using the solve function
  setinverse <- function(solve) inverse <<- solve
  
  ## Get the current value of the inverse field
  getinverse <- function() inverse
  
  ## The list containing the functionality
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  
  ## If there is a cached value of the inverse use this value
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  ## There was no cached inverse so calculate it
  data <- x$get()
  inverse <- solve(data, ...)
  
  ## Sets the cache to the calculated value
  x$setinverse(inverse)
  
  inverse
}

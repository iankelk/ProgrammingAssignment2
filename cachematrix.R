## These functions are for storing matrixes and caching their inverses.
## Week 3 Assignment for Coursera Data Science: R Programming

## Create a special matrix which can cache its inverse 
makeCacheMatrix <- function(x = matrix()) {
  # initialize the inverse
  inv <- NULL
  # define set function to store matrix in parent environment
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # define the get function 
  get <- function() x
  
  # set the inverse in the parent environment
  setinverse <- function(inverse) inv <<- inverse
  
  # return the value of the inverse
  getinverse <- function() inv
  
  # used to access functions with $ operator
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Returns the inverse of the matrix store by makeCacheMatrix. Calculates
## the inverse of the matrix, or if it has already been solved, returns
## the caches inverse
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  
  ## Check if matrix inverse was already cached
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## Matrix inverse was not cached, so compute, cache, and return inverse
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

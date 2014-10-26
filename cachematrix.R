## The script file implements two functions. 
## The first builds a special "matrix" able to cache its inverse. 
## The second computes the inverse of the spcial matrix if this isn't cached, 
## otherwise returns the precalculated inverse matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## Sets the matrix to invert initializing the cached invertedMatrix to NULL.
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## Returns the matrix to invert.
  get <- function() {
    x
  }
  ## Sets the inverted matrix.
  setInverse <- function(inv) {
    m <<- inv
  }
  ## Returns the inverted matrix if cached, NULL otherwise.
  getInverse <- function() {
    m
  }
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## Computes the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache.
## The function assumes that the input matrix is invertible.
cacheSolve <- function(x, ...) {
  ## Checks to see if the inverse of 'x' has already been calculated
  m <- x$getInverse()
  ## If the inverted matrix is not in the cache, 
  ##  computes it and adds it to the cache.
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  ##Compute the inverted matrix
  m <- solve(data)
  #Sets the value of the inverse in the cache
  x$setInverse(m)
  
  ## Return a matrix that is the inverse of 'x'
  m
}

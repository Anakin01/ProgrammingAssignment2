## This function creates a special "matrix" object that
## can cache its inverse

makeCacheMatrix <- function(x=matrix()) {
  ## Creates a list of functions that
  ## can cache the inverse of a matrix.
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() { x }
  setInverse <- function(inverse) { m <<-inverse }
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)  
}

## This function computes the inverse of the special matrix
## returned by makeCacheMatrix
## If the inverse of the matrix has already 
## been calculated, it is retrieved from the cache.
  
cacheSolve <- function(x, ...) {
  
  m <- x$getInverse()
  ## check if there is a matrix
  if ( ! is.null(m)) {
    print("getting cached data")
    return(m)
  }
  ## get the inverse of the matrix
  m <- solve(x$get())
  x$setInverse(m)
  m
}


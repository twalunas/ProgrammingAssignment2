## makeCacheMatrix and cacheSolve work together to create a matrix, 
## and store the inverse of the matrix in a cache that can be retrieved
## when cacheSolve is called on the same matrix more than one time

## makeCacheMatrix takes a matrix and stores it.  It has additional functions
## to reset the matrix and the cache of the inverse of the matrix as well
## as to get and set the inverse matrix/

makeCacheMatrix <- function(x = matrix()) {
  
  ## set up inverse matrix cache variable
  i <- NULL
  
  ## set up set, get, setinverse and getinverse functions
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve attempts to create the inverse of a matrix, but
## checks the object cache first to see if the matrix has already
## been computed.  If it has been computed, it returns the cached value
## otherwise, it computes the inverse, stores it in the cache and returns
## the cached values.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  
  ## check cache.  If cache is not empty, return it
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ## otherwise, get matrix, invert and set inverted matrix into cache
  m <- x$get()
  i <- solve(m, ...)
  x$setinverse(i)
  i
}

## The makeCacheMatrix and cacheSolve functions are used to create and reference
## a cached version of an inverse matrix defined by the user.  The inverse will 
## be computed and stored when calling the cacheSolve for the first time.
## Afterward, the invere matrix will be referenced from the "cache" rather than
## needing to compute each time.

## makeCacheMatrix creates list of functions used to create/access the original 
## matrix and the cached inverse (if applicable) of said matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
          x <<- y
          i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## cacheSolve function will check the cachematrix vector created from above
## and return the inverse if it has already been computed.  If it has not,
## the inverse of the matrix will be calculated then stored in the cachematrix
## for future reference.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
        message("getting cached data")
        return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

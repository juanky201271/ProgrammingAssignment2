## We store in cache a matrix and the inverse.
## The first time only store the matrix and its inverse, and the 
## next time just view the data store in the environment

## Store the matrix, and create the four functions for manage 
## the data stored (the matrix and the inverse)

makeCacheMatrix <- function(mat = matrix()) {
  inv <- NULL
  setmat <- function(mat2) {
    mat <<- mat2
    inv <<- NULL
  }
  getmat <- function() mat
  setinv <- function(inv2) inv <<- inv2
  getinv <- function() inv
  list(setmat = setmat, getmat = getmat,
       setinv = setinv, getinv = getinv)
  
}


## Do the Matrix's inverse, and manage the Environment's cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'mat'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$getmat()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

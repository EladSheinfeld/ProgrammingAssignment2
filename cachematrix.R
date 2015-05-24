## The following functions provides a way to store a matrix and cache its inverse.
## This is useful for cases in which the matrix or its inverse are needed multiple times.
## The current implementation assumes that the matrix is always invertible.

## This object stores the matrix and cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  get <- function()x
  
  setinverse <- function(inversedMatrix) m <<- inversedMatrix
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function compute the inverse of the matrix, assuming that 
## the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}

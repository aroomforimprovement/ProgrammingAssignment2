## The functions in this file are written to demonstrate
## matrix inversion as required by Programming Assignment 2
## from Coursera's R Programming

## This function creates a special "matrix" object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function returns the inverse of the matrix
## passed to it (preferably a cached version)

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

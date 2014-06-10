## This file has two functions to work with cached value for the inverse of a matrix.
## It´s the Programming Assignment 2 of the R Programming Course on Coursera started on 04/jun/2014

## makeCacheMatrix - creates a R object representing a matrix that can have it´s inverse cached
## It has 4 functions:
##  set       : set the matrix value
##  get       : returns the matrix value
##  setinverse: sets the inverse of the matrix
##  getinverse: returns the inverse of the matrix
## 
## examples: 
##          - creates the matrix
##            mat <- makeCachedMatrix() 
##
##          - set the matrix value
##            mat$set(matrix(c(1,3,3,1,4,3,1,3,4), nrow=3, ncol=3, byrow=TRUE))


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(invX) m <<- invX
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Calculates the inverse of the matrix created with the makeCachedMatrix function
## First test if the inverse is already avaiable. If yes, use it (getinverse). If not, calculates
## the inverse and store it in the cached matrix object (setinverse)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

## Data Science Specialization Track 
## R Programming  Course
## Programming Assignment 2  
##
## Inverting a matrix is computationally expensive. If the inverted matrix is cached then 
## it can be retrieved and this saves recomputation if the matrix has not changed.
##
## This file contains two mothods: 
## makeCacheMatrix - accepts a matrix as an argument and caches it.
## cacheSolve - acceps the cached matrix object and returns an inverted matrix.

## Usage :
## x <- matrix(c(1,-1/2,-1/2,1), nrow=2, ncol=2)
## m <- makeCacheMatrix(x) 
## xinv <- cacheSolve(m)
##
## Test : get back identity matrix:
## xinv %*% m$get()

## makeCacheMatrix : This function creates a special "matrix" object that can 
## cache its inverse. A list is returned that is used to exposes two setter 
## methods and two getter methods.
## set 		: set cached matrix uses << operator to allow assignment outside  
##		 	  current environment.
## get 		: return cached matrix
## setinverse : set inverse matrix
## getinverse : get inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve : This function computes the inverse of the matrix
## returned by  makeCacheMatrix  above. If the inverse has already been 
## calculated (and the matrix has not changed), then  cacheSolve   
## retrieves the inverse from the cache. Note not all matrices are invertable
## the matrix must at least be a square matrix.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv 
}

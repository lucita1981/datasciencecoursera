## Programming Assignment 2 - R Programming
## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than compute it repeatedly 
## This assignment is to write a pair of functions that cache the inverse of a matrix.

## 1. makeCacheMatrix: This function creates a special "matrix" object that 
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
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


## 2. cacheSolve: This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated  
## (and the matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache. This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
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

## Sample:
## > x <- matrix(2:5,2,2)     Creating matrix
## > m <- makeCacheMatrix(x)  Creating special matrix
## > m$get()                  Getting matrix
## [,1] [,2]
## [1,]    2    4
## [2,]    3    5
## > cacheSolve(m)            Calculating inverse matrix
## [,1] [,2]
## [1,] -2.5    2
## [2,]  1.5   -1
## > cacheSolve(m)            Calculating inverse matrix again
## getting cached data        Verifying it got from cache
## [,1] [,2]
## [1,] -2.5    2
## [2,]  1.5   -1


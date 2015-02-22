## Below are two functions that are used to create
## a special object that stores a matrix and cache's its inverse.

## Function makeCacheMatrix creates a matrix
## given as a list of the following functions:
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse matrix
## 4. get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Function cacheSolve calculates the inverse of the matrix
## created with function makeCacheMatrix
## It first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse matrix of the data and
## sets it in the cache via the setinv function.

cacheSolve <- function(x = matrix(), ...) {
  i<-x$getinv()
     if (!is.null(i)) {
      message("getting cached data")
      return(i)
  }
  data <- x$get()
  i <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
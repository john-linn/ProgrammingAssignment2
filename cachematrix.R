## Put comments here that give an overall description of what your
## functions do

## Adapt approach from makeVector to create a function list enabling
## a matrix's value to be provided and retrieved, and to cache and retrieve
## the solved value of the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
        ## initialize object to hold cached matrix and inverse
  mt <- NULL
  set <- function(y) {
    x <<- y
    mt <<- NULL
  }
  get <- function() x
  setinv <- function(solve) mt <<- solve
  getinv <- function() mt
  list (set=set, get=get, setinv=setinv, getinv=getinv)
}

## Adapt approach from cachemean to return the cached matrix if
## available, otherwise to compute, cache, and return it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mt <- x$getinv()
  if(!is.null(mt)) {
    message("getting cached data")
    return(mt)
  }
  data <- x$get()
  mt <- solve(data)
  x$setinv(mt)
  mt
}

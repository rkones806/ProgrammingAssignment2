## Put comments here that give an overall description of what your
## functions do

## Creates list for functions to set value, get value, set inverse, and get inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (y) {
    x <<- y
    inv <<- NULL
  }
  get <- function () x
  setinv <- function(solve) inv <<- solve
  getinv <- function () inv
  list( set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}


## Returns previously calced inverse.  If not previously calced, cacheSolve calcs inverse and then sets it into the cache 
##  with x$setinv code.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinv(inv)
}
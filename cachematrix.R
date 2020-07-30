## Due to matrix inversion is a costly computation, I tried to execute 
## two functions to cache the inverse of a matrix. In this way, inverse of matrix might be avoided to repeat 
## by using these functions

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function()
    x
  setinv <- function(inverse)
    inv <<- inverse
  getinv <- function()
    inv
  list(
    set = set,
    get = get,
    setinv = setinv,
    getinv = getinv
  )
}
##The function we created figures out the inverse of the special "matrix"  returned by 
## makeCacheMatrix made above. If the inverse has already been calculated 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  smatrix <- x$get()
  inv <- solve(smatrix, ...)
  x$setinv(inv)
  inv
}


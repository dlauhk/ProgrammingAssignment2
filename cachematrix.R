## Put comments here that give an overall description of what your
## functions do
## [DL] The functions caches the inverse of matrix for more effective computation
## Write a short comment describing this function
## [DL] This function is to create the matrix and cache the value
makeCacheMatrix <- function(x = matrix()) {
  sol <- NULL
  set <- function(y) {
    x <<- y
    sol <<- NULL
  }
  get <- function() x
  setsol <- function(solve) sol <<- solve(x)
  getsol <- function() sol
  list(set = set,
       get = get,
       setsol = setsol,
       getsol = getsol)
}

## Write a short comment describing this function
## [DL] This function is to make use of the cache value for computing solerse of Matrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  sol <- x$getsol()
  if(!is.null(sol)) {
    message("getting cached data")
    return(sol)
  }
  data <- x$get()
  sol <- solve(data, ...)
  x$setsol(sol)
  sol
}

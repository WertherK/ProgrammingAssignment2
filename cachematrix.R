## Function “makeCacheMatrix” creates a special “matrix” object that can cache its inverse. makeCacheMatrix contains 4 functions: set, get, setinverse, getinverse.

## get is a function that returns the vector x stored in the main function.
## set is a function that changes the vector stored in the main function.
## setinverse is a function that solve the matrix.
## getinverse is a function that returns the vector inversed
## they simply store the value of the input in a variable m into the main function makeCacheMatrix (setinverse) and return it (getinverse).

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Function “cacheSolve” computes the inverse of the special “matrix” (which is the input of cacheSolve) returned by makeCacheMatrix

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

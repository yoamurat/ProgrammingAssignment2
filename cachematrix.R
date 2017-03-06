## Set of functions to compute the inverted matrix enabling storage in Cache to avoid large recomputing time

## makeCacheMatrix returns a list of functions enabling setting a different value to the matrix
## storing the inverted matrix in cache, and getting both results


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve returns the inverted matrix of a matrix computed in makeCacheMatrix 
## it checks if the result is available in Cache before re-calculating

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}


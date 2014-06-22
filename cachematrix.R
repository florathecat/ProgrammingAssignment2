## makeCacheMatrix sets a matrix (x), gets a matrix, sets the inverse of the matrix, and gets the inverse of the matrix
## cacheSolve checks to see if the inverse of x is in the cache. If so, it retrieves it with a message; if not, it calculates it.

makeCacheMatrix <- function(x = matrix()) {
  ## replacing in machVector sample script with the following
  ## m -> x_inv, function(mean) -> function(solve)
  ## setmean -> setinv; getmean -> getinv
  x_inv <- NULL
  set <- function(y) {
    x <<- y
    x_inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) x_inv <<- solve
  getinv <- function() x_inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}



cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## substitute in cachMean "m" with "x_inv", "setmean" with "setinv" and "getmean" with "getinv"      ## Return a matrix that is the inverse of 'x'
  x_inv <- x$getinv()
  if(!is.null(x_inv)) {
    message("getting cached data")
    return(x_inv)
  }
  data <- x$get()
  x_inv <- solve(data, ...)
  x$setinv(x_inv)
  x_inv
}

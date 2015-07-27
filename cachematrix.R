##The following function makeCacheMatrix creates a special matrix which contains
##different functions to set and get the matrix and set and get its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## holds the cached value (initially NULL)
  inv <- NULL
  ## store a matrix
    set <- function(y) {
    x <<- y
    inv <<- NULL
  }
    ## return stored matrix
  get <- function() x
  ## cache argument 
  setinv <- function(solve) inv <<- solve
  ## get cached value
  getinv <- function() inv
  ## return list. 
  list(set = set, get = get,
       setinv = setinv,
       getinv= getinv)
}


## The function below calculates the inverse of the matrix if it has already not
## been calculated.If calculated, it uses the calculated value directly.

cacheSolve <- function(x, ...) {
  ## get cached value
  inv <- x$getinv()
  ## return value if cached value is there
  if(!is.null(inv)) {
    message("getting cached matrix...")
    return(inv)
  }
  ## else get the matrix and caclulate the inverse, store it in the cache
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  ## return inverse
  inv
}

#### Pair of functions that cache the inverse of a matrix.
#### It is assumed that the matrix supplied is always invertible.

## `makeCacheMatrix` creates a special matrix, which is basically a list containing functions to:
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the matrix inverse
## 4.  get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  x_inv <- NULL
  set <- function(y) {
    x <<- y
    x_inv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) x_inv <<- inv
  getinv <- function() x_inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## `cacheSolve` calculates the inverse of the special matrix created via `makeCacheMatrix`.
## However, it first checks to see if the inverse has already been calculated. 
## If so, it `get`s the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the `setinv` function.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("Getting cached data...")
    return(inv)
  }
  mtrx <- x$get()
  inv <- solve(mtrx, ...)
  x$setinv(inv)
  inv
}

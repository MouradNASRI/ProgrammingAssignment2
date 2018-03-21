## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(X = matrix()) {
  inv_mat <- NULL
  set <- function(Y) {
    X <<- Y
    inv_mat <<- NULL
  }
  get <- function() X
  setinv <- function(solve) inv_mat <<- solve
  getinv <- function() inv_mat
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(X, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_mat <- X$getinv()
  if(!is.null(inv_mat)) {
    message("getting cached data")
    return(inv_mat)
  }
  data <- X$get()
  inv_mat <- solve(data, ...)
  X$setinv(inv_mat)
  inv_mat
}

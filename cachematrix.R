## The functions within this file will find the inverse of a given matrix (if possible), cache it, then retrieve the cached
# inverse matrix from the cache if has already been calculated.  

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv_mat <- function(inverse) i <<- inverse
  getinv_mat <- function() i
  list(set = set, get = get,
       setinv_mat = setinv_mat,
       getinv_mat = getinv_mat)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has 
# already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv_mat()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv_mat(i)
  i
}



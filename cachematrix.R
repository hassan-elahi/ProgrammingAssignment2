## Put comments here that give an overall description of what your
## functions do 
## This function cach the invert of matrix 

## Write a short comment describing this function
## This Function can:
##set the value of the matrix
##get the value of the matrix
##set the value of the matrix
##get the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
  r <- NULL
  set <- function(y) {
    x <<- y
    r <<- NULL
  }
  get <- function() x
  setinv <- function(reverse) r <<- reverse
  getinv <- function() r
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## Write a short comment describing this function
##If the inverse has already been calculated (and the matrix has not changed)
##, then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data) %*% data
  x$setinv(m)
  m
}

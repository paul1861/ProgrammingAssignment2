## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix for calculation of the inverse. 
## This returns a list for athe cacheSolve function.
##
makeCacheMatrix <- function(x = matrix()) {
  ## Set local variable inv to NULL
  inv = NULL
  
  ## Create the set, get, setinv, and getinv methods
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}



## This function uses makeCacheMatrix above and checks to see if the 
## input matrix is cached and either returns the cached inverted 
## matrix (inv) or computes it directly.
##
cacheSolve <- function(x, ...) {
  ## Set the inv to the inverse of x using makeCacheMatrix
  inv = x$getinv()
  
  ## If it already exists in cache, get it
  if (!is.null(inv)){
    message("Exists in cache...")
    return(inv)
  }
  
  ## Otherwise, use solve to calculate inverse
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  x$setinv(inv)
  return(inv)
}

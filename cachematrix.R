## Put comments here that give an overall description of what your
## functions do

## Creates a matrix for further calucation of the inverse. This function checks to see
## if the cached matrix inverse exists, otherwise it uses the setInv function
## 
makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}



## This function returns the inverse matrix of the data from makeCacheMatrix

cacheSolve <- function(x, ...) {
  inv = x$getinv()
  
  if (!is.null(inv)){
    message("Get cached data.")
    return(inv)
  }
  
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  x$setinv(inv)
  
  return(inv)
}

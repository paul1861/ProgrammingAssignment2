## Put comments here that give an overall description of what your
## functions do

## Creates a matrix for further calucation of the inverse. This function checks to see
## if the cached matrix inverse exists, otherwise it uses the setInv function

makeCacheMatrix <- function(x = matrix())
{
     invrs = x$getinv()
     
    if (!is.null(invrs))
    {
	message("getting cached data")
	return(invrs)
    }
     
     # Calculate inverse
     mat.data = x$get()
     invrs = solve(mat.data, ...)
     
     # Sets value of the inverse in the cache via the setinv function.
     x$setinv(invrs)
     
     return(invrs)
}


## This function returns the inverse matrix of the data from makeCacheMatrix

cacheSolve <- function(x, ...)
{
     invrs = NULL
     set = function(y) 
     {
         x <<- y
         invrs <<- NULL
     }
     get = function() x
     setinv = function(inverse) invrs <<- inverse 
     getinv = function() invrs
     list(set=set, get=get, setinv=setinv, getinv=getinv)
}

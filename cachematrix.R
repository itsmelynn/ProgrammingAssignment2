## The first function makes a cache data and the second function returns an inverse
## matrix of the given argument. If the result is saved in the cache data, the function
## will not calculate the inverse, but just return the result from the cache.

## This function makes an inverse matrix cache data.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
          x <<- y
          i <<- NULL
          }
  get <- function()x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function()i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## When you put a matrix x as a argument,
##this function returns the inverse matrix of x.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
                }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

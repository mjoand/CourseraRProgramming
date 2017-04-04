
## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## The special "matrix" object returned by makeCacheMatrix is a list composed of 4
## functions: set, get, setinverse and getinverse.
## 
## get: caches the matrix x, passed as an argument to the MakeCacheMatrix function
## getinverse: returns NULL, when MakeCacheMatrix is called for the first time 
 # or if MakeCacheMatrix is called withou a subsequent call of the function cacheSolve,
 # or caches the inverse of x when MakeCacheMatrix is called following a 
 #call from cacheSolve.


makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y) {
      x <<- y
      inv <<- NULL
    }
get <- function() {x}
setinverse <- function(inverse) {inv <<-inverse} 
getinverse <- function() {inv}
list(set = set, get = get,setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), 
# cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
#verifying if the inverse matrix has been cached and returning it if that is the case
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }

##If the inverse matrix has not been cached the original matrix x is retrieved from
 # the cache function get, its inverse is computed and passed as an argument to
 # the function set inverse so that it can be cached by getinverse in makeCacheMatrix

  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}


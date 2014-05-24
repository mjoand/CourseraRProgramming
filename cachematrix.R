
## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## The special "matrix" object returned by makeCacheMatrix is a list composed of 4
## functions:
## set: 
## get: returns the matrix x, passed as an argument to the MakeCacheMatrix function
## setinverse:
## getinverse: returns either NULL, when MakeCacheMatrix is called for the first time,
 #           or the inverse matrix if MakeCachematrix is called subsequently with
 #           the same matrix x as argument


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
  
 ## retrieving inv (NULL if it is the first time the 
  # sequence cachesolve(MakeCacheMatrix) has been called,
  # the cached inverse matrix if cachesolve(MakeCacheMatrix)
  #has been previously called with the same matrix as argument
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}


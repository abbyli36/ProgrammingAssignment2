## These are the functions we have been asked to make for Programming Assignment 2 
## for the R programming course 

## This function creates a special "matrix" object that can cache its inverse. 
## Note that this function assumes that the matrix is a square matrix and is 
## inversible

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL  ##inv will store the inversed matrix
      ##set will change your matrix if you want it changed from the input
      set <- function(y){ 
            x <<- y
            inv <<- NULL
      }
      ##get will retrive your matrix
      get = function() x
      ##setinverse will set the inverse of your matrix
      setinverse = function(inverse) inv <<- inverse
      ##getinverse will get the inverse of your original matrix
      getinverse = function() inv
      ##list stores the four functions within the makeCacheMatrix functino
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinverse()
      ## if the inverse is already calculated, return the cached value
      if (!is.null(inv)){
            message("getting cached data")
            return(inv)
      }
      ## if the inverse is not already calculated, then calculate the inverse
      data <- x$get() ##retrive original matrix from makeCacheMatrix
      inv <- solve(data,...) ##inverse the original matrix
      x$setinverse(inv) ##caches the inversed matrix
      inv ## returns inversed matrix
}

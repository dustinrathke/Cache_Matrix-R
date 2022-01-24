##Caching the Inverse of a Matrix
##Due to the intensive requirements of matrix inversion
##these functions are used to create a special object
##that cahces the inverse of a matrix and stores it.

##This function will create a special matrix and cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      set <- function(y) {
        x <<- y
        inv <<- NULL
      }
      get <- function()x
      setInverse <- function(inverse) inv <<- inverse
      getInverse <- function() inverse
      list(set = set,
           get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## This function computes the inverse of the special matrix created
## by the last function. It should retrieve the inverse from the cahce
## if it has already been completed.

cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()
        if (!is.null(inverse)) {
          message("Retrieving cache")
          return(inverse)
        }
        z <- x$get()
        inverse <- solve(z, ...)
        x$setInverse(inverse)
        inverse
}

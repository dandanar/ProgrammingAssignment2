## This script provides function to calculate the inverse of a matrix
## and caches that inverse so it can be called again without recalculating.

## The first function creates a matrix in a way that facilitates caching.

makeCacheMatrix <- function(x = matrix()) {
     #Initialize m as blank.
     m <- NULL
     #Set the vector to be inversed in the local environment
     #And also clear the value m of any previously calculated inverse.
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     #Stores the matrix x
     get <- function() x
     ##Set the vector m, a function to be called by cacheSolve
     setinv <- function(inv) m <<- inv
     ##Retrieves the inverse matrix, m
     getinv <- function() m
     #function returns a list of functions to use in cacheSolve.
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)

}


## This function either calculates the inverse of the matrix or
## if the inverse has already been calculated, returns that inverse again.
## Note that you must pass the output of makeCacheMatrix to cacheSolve
## Not just the matrix x itself (so that cacheSolve can access the functions)

cacheSolve <- function(x, ...) {
     #First, m retrieves either a null value or a stored inverse.
     m <- x$getinv()
     #If m is not null, we return the stored inverse and stop.
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     #if m is null, we continue to retrieve the data and calculate its inverse
     data <- x$get()
     m <- solve(data, ...)
     #Then we call setinv from above to set the value of m in the cache.
     x$setinv(m)
     #And finally we return the newly calculated inverse value.
     m
}

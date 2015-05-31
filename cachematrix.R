## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates a class-like structure to store a matrix with a cached inverse product. Similar to a matrix
# object.
cacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set <- function(y) {
        if(nrow(y) != ncol(y)) {
            message("Matricies don't have the sme row and cols..")
        }
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() x
    setinverse <- function(solve) inverse <<- solve
    getinverse <- function() inverse
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Write a short comment describing this function
# This function demonstrate the use of a matrix object cache. It will return a newly calculated invers of the matrix or
# will use it's cached value, if it exists.
cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse) && !is.na(inverse)) {
        message("Cache hit.")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}

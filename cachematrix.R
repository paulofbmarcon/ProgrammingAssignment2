##
##
## Programming Assignment 2
## Caching the Inverse of a Matrix
##
## Modified from the original example:
## Caching the Mean of a Vector
##
##


## Create a special object to store the data
## of the original matrix, and its inverse, 
## if it was already calculated.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setMatrix <- function(matrix) m <<- matrix
    getMatrix <- function() m
    list(set = set, get = get,
         setMatrix = setMatrix,
         getMatrix = getMatrix)
}


## Verify if the inverse matrix was already calculated and return the value of cache or 
## calculate the inverse matrix, do the cache and return the value.
## This function is only valid for objects created with the previous function
cacheSolve <- function(x, ...) {
    m <- x$getMatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setMatrix(m)
    m
}

## This file contains a pair of functions that cache the inverse of a matrix.
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of
## a matrix rather than computing it repeatedly. 

## Creates a "matrix" object that can cache its inverse. 
makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    
    # Set method
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    # Get method
    get <- function() {
        # returns the matrix
        x
    }
    
    # Sets the inverse matrix 
    setInverseMatrix <- function(local_variable) m <<- local_variable
    
    # Gets the inverse matrix 
    getInverseMatrix <- function() m
    
    # Returns methods
    list(set = set, get = get,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
    }

## Computes the inverse of the special "matrix" returned by makeCacheMatrix() function above. If the inverse has already
## been calculated (and the matrix has not changed), then cacheSolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
    
    # Gets the inverse matrix x
    m <- x$getInverseMatrix()
    
    # Checks whether the inverse matrix exists already, if yes then it returns it and exits the function
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # Retrieves the matrix
    data <- x$get()
    
    # solve(c) function gives the correct inverse. You need to use %*%  operator to invoke matrix multiplication in R.
    m <- solve(data) %*% data
    
    x$setInverseMatrix(m)
    
    # Returns the inverse matrix
    m
}

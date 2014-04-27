## This file contains the code to fulfill the requirements of Coursera R
## Programming course's Programming Assignment #2

## It contains two functions: (1) makeCacheMatrix that creates a special "matrix" 
## object that can cache its inverse, and (2) cacheSolve that computes the 
## inverse of the special "matrix" returned by makeCacheMatrix. If the inverse 
## has already been calculated, then cachesolve retrieves the inverse from the 
## cache.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {                            # Sets the value of a matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x                             # Gets the value of a matrix
        setInverse <- function(Inverse) m <<- Inverse   # Sets the value of the matrix's inverse
        getInverse <- function() m                      # Gets the value of the matrix's inverse
        list(set = set, get = get,                      # A list of the four defined functions
             setInverse = setInverse,
             getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated, then 
## cachesolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {                       # Return a matrix that is the inverse of 'x'
        m <- x$getInverse()                            # Query the x matrix's cache
        if(!is.null(m)) {                              # If there is a cache
                message("getting cached data")
                return(m)                              # Return the cache with no computation
        }
        data <- x$get()                                # If no cache
        m <- solve(data, ...)                          # Compute the inverse
        x$setInverse(m)                                # Save the result to x's cache
        m                                              # Return the result
}

## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(Inverse) m <<- Inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

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

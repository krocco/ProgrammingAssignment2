## The following functions serve to cache the inverse of a matrix
## this is useful due to the time required to invert large matrices

## The following function creates a special matrix, which is actually a 
## list containing a function which
## sets the value of the matrix
## gets the value of the matrix
## sets the value of the inverse of the matrix
## gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        invrs <- NULL
        set <- function(y) {
                x <<- y
                invrs <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invrs <<- inverse
        getinverse <- function() invrs
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function solves the inverse of a matrix after first 
## checking whether the inverse has already been solved. If it has
## it skips the solve and returns the result. If it has not previously
## been solved, it solves the inverse and cahes it.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(invrs)
        }
        data <- x$get()
        invrs <- solve(data, ...)
        x$setinverse(invrs)
        invrs
}

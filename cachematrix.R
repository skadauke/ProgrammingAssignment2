## Matrix inversion can be computationally costly when performed repeatedly. 
## The following functions allow caching of the matrix inversion computation. 

## This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    # Construct the matrix object with an invalid inversion cache
    i <- NULL
    
    # Set the value of the matrix and invalidate inversion cache 
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    # Get the value of the matrix
    get <- function() x
    
    # Set the value of the inverse
    setinverse <- function(inv) i <<- inv
    
    # Get the value of the inverse
    getinverse <- function() i
    
    # Return a list of accessor methods
    list(set = set, get = get,
         setinverse = setinverse, getinverse = getinverse)
}


## This function sets and returns the inverse of a matrix object generated 
## by makeCacheMatrix. 
## If the inverse has already been calculated, and the matrix has not changed,
## then it will retrieve the inverse from the cache. Otherwise, it computes
## the inverse and stores it inside the matrix object.

cacheSolve <- function(x, ...) {
    # Check if the matrix object has a cached inverse
    i <- x$getinverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    # No cached inverse present, so we have to calculate it
    data <- x$get()
    i <- solve(data)
    
    # Cache the result and return it
    x$setinverse(i)
    i
}

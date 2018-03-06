##
## Caching the Inverse of a Matrix
##

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
	
    # Sets the initial matrix and resets the inverse matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
	
    # Gets the initial matrix
    get <- function() x
	
    # Sets the inverse matrix
    setinverse <- function(im) i <<- im
	
    # Gets the inverse matrix from cache
    getinverse <- function() i
    
    # Returns the special "matrix"
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    # Gets the inverse matrix from cache and checks if it is not null
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
	
	# Gets the matrix value and calculates the inverse
    data <- x$get()
    i <- solve(data, ...)
	
	# Sets the inverse matrix into cache and return it
    x$setinverse(i)
    return(i)	
}

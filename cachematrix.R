
# Creates a CacheMatrix object in the form of a list with
# accessors and mutators for the original and inverse matrix.  The calculation
# is *NOT* performed here, this is nothing but a cache.  For information on
# calculating the solution/inverse matrix refer to cacheSolve.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x

    setinverse <- function(inv){
        i <<- inv
    }
    getinverse <- function() i
    
    list(set = set, 
         get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}

# Solves a system of equations explicitly created by the function 
# makeCacheMatrix.  If the inverse/solution has been calculated before, it
# will return the cached value, otherwise it will calculate the new solution
# and set it using the functions available to the object 'x'.
cacheSolve <- function(x, ...) {
    
    inv <-x $getinverse()
    ## If the inverse matrix has been calculated, just return it
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
        
    ## If there isn't a solution yet, calculate it, then cache it.
    message("cache was empty, calculating...")
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
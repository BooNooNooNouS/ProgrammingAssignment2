## 


## Creates a CacheMatrix object in the form of a list with
## accessors and mutators for the original and inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x

    setinverse <- function(inv) i <<- inv
    getinverse <- function() i
    
    list(set = set, 
         get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}

## Solves a system of equations explicitly created by the function 
## makeCacheMatrix
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
## This assignment consists on writing a pair of functions that cache the inverse of a matrix

## makeCacheMatrix creates a special "matrix" object (a list) that stores a matrix and can cache
## its inverse, although it does not calculate it.
  
makeCacheMatrix <- function(x = matrix()) {
        ## Args:
        ##       x: is the matrix which inverse we want to cache
        ## Returns:
        ##       A list of 4 functions: set, get, setinv and getinv
        ##       set: sets the value of the matrix
        ##       get: gets the value of the matrix
        ##       setinv: sets the value of the inverse
        ##       getinv: gets the value of the inverse
        
        inv <- NULL
        set <- function(y) {  ## no need to use set unless we want to change the matrix 
                x <<- y       ## substitutes x with y (the new matrix) in the main function
                inv <<- NULL  ## restores to NULL the value of the inverse because the old
                              ## inverse is not needed anymore. The new inverse needs to
                              ## be recalculated by cacheSolve
        }
        get <- function() x  ## returns the matrix x stored in the main function
        setinv <- function(inverse) inv <<- inverse  ## stores the value of the imput (inverse)
                                                     ## into a variable (inv) in the main function
                                                     ## setinv doesn´t compute the inverse of x.
                                                     ## If we set a value with setinv, even if it
                                                     ## is not the inverse of x, it will be 
                                                     ## retrieved by getinv
        getinv <- function() inv  ## returns the value of inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)  ## stores the 4 functions of
                                                                      ## makeCacheMatrix
        
}  


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix 
## unless it has previously been calculated and stored in the cache.
## If the inverse has already been calculated (and the matrix has not changed),
## cacheSolve returns the inverse stored in the cache. Imput of cacheSolve is the object
## where makeCacheMatrix is stored.

cacheSolve <- function(x, ...) {
        ## Args:
        ## 		x: is the matrix wich inverse we want to compute
        ##
        ## Returns:
        ## 		the inverse of x, either calculated or retrieved from the cache 
        ## 		if it had already been calculated
        
        
        inv <- x$getinv()  ## cacheSolve assigns to inv the value stored previously with getinv
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)  ## cacheSolve verifies that inv exists and is not NULL.
                             ## If it exists in memory, returns a message and its value (that is)
                             ## supposed to be the inverse, but not necessarily because we could
                             ## have changed it with setinv
        }
        data <- x$get()  ## If there is no inverse in memory, cacheSolve calculates it.
                         ## Data gets the matrix stored with makeCacheMatrix,
        inv <- solve(data, ...)  # inv calculates the inverse of the matrix
        x$setinv(inv)  ## x$setinv(inv) stores it in the object generated assigned with makeCacheMatrix
        inv  ## returns the inverse
}


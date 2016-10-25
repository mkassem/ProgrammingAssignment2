## Put comments here that give an overall description of what your
## functions do

## create a function that allow matrix to save its inversion, and retreive it from cache when required.

makeCacheMatrix <- function(x = matrix()) {
        #define element that will store cached inversion
        inv <- NULL
        
        #set the matrix value
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        #return the amtrix
        get <- function() x
        
        #save matrix inversion to 
        setinversion <- function(z) inv <<- z
        #retreive matrix inversion from cache
        getinversion <- function() inv
        
        #retrun list contain new matrix elements and function
        list(set = set, get = get,
             setinversion = setinversion,
             getinversion = getinversion)
}


## check our new matrix object has cached value or not, if had return it else calculate it and set to the cache for next requested.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinversion()
        
        #check if the matrix inversion cache is null
        if(!is.null(inv)) {
                message("getting cached data")
                #return inversion from the cache
                return(inv)
        }
        
        #inversion the matrix, store in cache, return the inversion
        data <- x$get()
        inv <- solve(data, ...)
        x$setinversion(inv)
        inv
}

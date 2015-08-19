## A pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its 
## inverse

makeCacheMatrix <- function(x = matrix()) {
    ## Holds the cached value or NULL if nothing is cached like initially
    inv <- NULL
    ## Store a matrix
    set <- function(y) {
        x <<- y
        ## If the matrix is assigned a new value, clean the cache
        inv <<- NULL
    }
    ## Retreive the stored matrix
    get <- function() x
    ## Calculte the inverse with solve and cache the value
    setinv <- function(solve) inv <<- solve
    ## Retrieve the cached value
    getinv <- function() inv
    ## Retrieve a list in wich each element of the list is a function
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix  above. If the inverse has already been calculated (and
## the matrix has not changed), then it should retrieve the inverse from 
## the cache.

cacheSolve <- function(x, ...) {
    ## Retrieve the cached matrix
    inv <- x$getinv()
    ## If a cached matrix exists return it 
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    ## Otherwise get the matrix, calculate the inverse with Solve function
    ## and store it in the cache
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    ## Return a matrix that is the inverse of 'x'
    inv
}

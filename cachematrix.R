## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    ## Initialization
    inv <- NULL
    ## Set matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    ## Get the matrix
    get <- function() x
    ## Set the inverse of matrix
    setInverse <- function(inverse) inv <<- inverse
    ## Get the inverse of matrix
    getInverse <- function() inv 
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    ## Check whether the inverse of matrix is available
    if(!is.null(inv)) {
        message("getting cached data")
        ## Return the inverse of matrix
        return(inv)
    }
    ## Get matrix
    data <- x$get()
    ## Calculate the inverse of matrix
    inv <- solve(data, ...)
    ## Set the inverse of matrix
    x$setInverse(inv)
    ## Resurn the inverse of matrix
    inv
}

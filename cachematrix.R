## This file contains R functions that calculate and cache the inverse of a 
## specified matrix.  

## The makeCacheMatrix function creates a special matrix, which has the additional 
## functions of get, set, getInverse, and setInverse functions.  The getInverse and 
## setInverse functions get and set the inverse of the specified matrix.

makeCacheMatrix <- function(x = matrix()) {
    matrixInverse <- NULL         ## store the inverse of the matrix 
    
    ## set function:  set (or reset) the matrix.
    set <- function(newMatrix) {
        x <<- newMatrix 
        matrixInverse <<- NULL    ## reset any cached inverse of original matrix 
    }
    
    ## get function:  get the matrix.
    get <- function() {
        x 
    }
    
    ## setInverse function:  set the inverse of the matrix.
    setInverse <- function(newInverse) {
        matrixInverse <<- newInverse
    }
    
    ## getInverse function:  get the inverse of the matrix. 
    ## This returns NULL if the inverse has not been set yet.
    getInverse <- function() {
        matrixInverse
    }
    
    ## return the list of supported functions on the matrix
    list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)
}


## The cacheSolve function will calculate the inverse of a matrix, and cache it 
## to improve performance.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'  
    
    ## check if the matrix inverse is already set
    matrixInverse <- x$getInverse()
    if(!is.null(matrixInverse)) {
        message("getting cached inverse")
        return(matrixInverse)
    }
    
    ## must calculate the matrix inverse, using R's solve function.
    origMatrix <- x$get()
    matrixInverse <- solve(origMatrix, ...)
    x$setInverse(matrixInverse)
    matrixInverse    
}

## makeCacheMatrix and cacheSolve functions provide functionality for
## calculating and caching the inverse of the matrix to avoid re-calculation

## makeCacheMatrix function is a wrapper for matrix that provides
## four interface functions to set and get the cached value of the
## inverted matrix and the matrix itself

makeCacheMatrix <- function(x = matrix()) 
{
    # set default value for inverted matrix
    invCache <- NULL
    # getter and setter function for inverted matrix cache
    getInverted <- function() invCache
    setInverted <- function(inv) invCache <<- inv
    # getter and setter function for the matrix itself
    getMatrix <- function() x
    setMatrix <- function(m)
    {
        x <<- m
        invCache <<- NULL
    }
    # return the list of four above functions
    list (setInverted = setInverted,
          getInverted = getInverted,
          setMatrix = setMatrix,
          getMatrix = getMatrix)
}


## cacheSolve function calculates and caches the inverse of the matrix only once
# other times it returns previously cached value instead of recalculating it again

cacheSolve <- function(x, ...) 
{
    inv <- x$getInverted()
    # return the cached inverted matrix if it is not empty(NULL)
    if (!is.null(inv))
    {
        message("getting cached inverted matrix")
        return(inv)
    }
    # otherwise cacluate the inverted matrix
    m <- x$getMatrix()
    inv <- solve(m, ...)
    # cache the calculated inverse of the matrix
    x$setInverted(inv)
    ## Return a matrix that is the inverse of 'x'
    inv
}
##test case
#matrix(1:4,2,2) -> m
#makeCacheMatrix(m) -> cm
#cacheSolve(cm)
#cacheSolve(cm)
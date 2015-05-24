# myhashtagislbb
## functions included in this file:
##  makeCacheMatrix - creates a special "matrix" object that can cache its inverse
##  cacheSolve - computes the inverse of the "matrix" returned by makeCacheMatrix


# makeCacheMatrix will return options for user to set and set the matrix. 
#   It will also return options for the user to get and set the inverse of the matrix.
#   1 - set the matrix
#   2 - get the matrix
#   3 - set the inverse
#   4 - get the inverse
# These options are used as inputs into cacheSolve listed below

makeCacheMatrix <- function(x = matrix()) {
    inv = NULL
    set = function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse 
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}



## cacheSolve will return the inverse of the original matrix input to makeCacheMatrix

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    
    # if the inverse has already been calculated, return the inverse
    if (!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    
    # else, calculate the inverse 
    mat.data <- x$get()
    inv <- solve(mat.data, ...)
    
    # sets the value of the inverse in the cache via the setinv function.
    x$setinv(inv)
    
    return(inv)
}

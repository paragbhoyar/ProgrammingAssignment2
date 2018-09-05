## makeCacheMatrix is a function that takes a matrix as its argument
## and store its inverse in a cache memory. It return four functions
## get, set, setinverse & getinverse as a list of characters.

makeCacheMatrix <- function(x = matrix()) {
    x_inv <- NULL
    set <- function(y){
        x <<- y
        x_inv <<- NULL
    }
    get <- function(){x}
    setinverse <- function(solve){ x_inv <<- solve(x)}
    getinverse <- function(){x_inv}
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve retrieves 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    x_inv <- x$getinverse()
    if(!is.null(x_inv)){
        message("getting cached data")
        return(x_inv)
    }
    data <- x$get()
    x_inv <- solve(data, ...)
    x$setinverse(x_inv)
    x_inv
}

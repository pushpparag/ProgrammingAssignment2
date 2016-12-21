## This file contains functions to calculate the inverse of a matrix and cache it.
## If a cached copy is present, then it is pulled instead of doing the same calculations all over again.

## The makeCacheMatrix function deals with the actual cached data.
## It has 4 objects which, when called by cacheSolve function, either retrieve or set a cached copy of the matrix/inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set<-function(y){
        x<<-y
        inv<<-NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}


## The cacheSolve function either calculates the inverse of the matrix,
## or if a cached copy is present, retrieves it.
## It does so by calling the objects (functions) created by makeCacheMatrix function.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}

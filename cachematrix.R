## The following two functions calculate inverse of a matrix.  The inverse is
## stored in the cache so to minimize computation time the next time inverse
## of the same matrix will be used.

## The function makeCacheMatrix creates a special "matrix" object that can
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    set <-function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## The function cacheSolve computes the inverse of the special "matrix"
## created by makeCacheMatrix.  If the cached inverse is available,
## cacheSolve retrieves it, otherwise, it computes, caches, and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("getting chached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}

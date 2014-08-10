## Put comments here that give an overall description of what your
## functions do

## Creates speial matrix that caches its inverse 
## matrix is obtained with mylist[2]$get()

makeCacheMatrix <- function(x = matrix()) {
 invmatrix <- NULL
        set <- function(y) {
                x <<- y
                invmatrix <<- NULL
        }
        get <- function() x
        setinv <- function(solve) invmatrix <<- solve
        getinv <- function() invmatrix
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## Function that calculates inverse matrix or retrieve it from cache

cacheSolve <- function(x, ...) {
        invmatrix <- x$getinv()
        if(!is.null(invmatrix)) {
                message("getting cached data")
                return(invmatrix)
        }
        data <- x$get()
        invmatrix <- solve(data,diag(ncol(data)), ...)
        x$setinv(invmatrix)
        invmatrix

}

## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix sets a list of 4 functions. The first one sets the matrix,
## the second one recovers the input matrix, the third one caches the inverse,
## and the last one recovers the cached inverse

## Note that if you give a matrix as the argument, you don't need to use the
## set function

makeCacheMatrix <- function(x = matrix()) {
        
        ## The variable inv will cache the inverse. It's declared NULL when we
        ## computed the inverse.
        inv <- NULL
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve finds the inverse of a matrix. If the inverse has already been
## computed, the functions returns the cached inverse

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        input_matrix <- x$get()
        inv <- solve(input_matrix, ...)
        x$setinv(inv)
        inv
}

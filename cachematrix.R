## Functions makeCacheMatrix and cacheSolve are used to compute and cache matrix inverse.
## If the matrix is not changed, inverse can be looked up in the cache rather than recomputed.
## This is useful when matrix inverse has to be computed repeatedly because it will speed up computation.

## makeCacheMatrix function is used to construct special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(data) {
                x <<- data
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(data) inverse <<- data
        getinverse <- function() inverse
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix function.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse = x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                inverse
        } else {
                matrix <- x$get()
                inverse <- solve(matrix)
                x$setinverse(inverse)
                inverse
        }
}

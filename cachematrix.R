## These functions cache the inverse of a matrix 
## so we can retrieve it from cache rather than
## recomputing each time we need it. This saves time.

## The first function creates a cachable matrix by
## setting the value of the matrix and getting that value,
## then setting the value of the inverse matrix
## and getting that value.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}

## The second function calculates the inverse of the 
## matrix returned in the first function. If the inverse
## was previously calculated and the matrix is unchanged,
## this function retrieves the inverse from cache.

cacheSolve <- function(x, ...) {
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setmatrix(m)
        m
}
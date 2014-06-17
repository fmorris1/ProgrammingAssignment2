## The following pair of functions caches the inverse of a matrix.
## The functions creates a special object that stores a matrix and caches
## its inverse

## The makeCacheMatrix function creates a special vector, which is a list
## containing a function to:
##
## (1) Set the values of the matrix
## (2) Get the values of the matrix
## (3) Set the values of the matrix inverse
## (4) Get the values of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        setMatrix <- function(y) {
                x <<- y
                m <<- NULL
        }
        getMatrix <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The following function calculates the inverse of the special "matrix" created
## with the above function. It first checks to see if the inverse has already been
## calculated. If so, it gets the martrix from matrix inverse from the cache and 
## skips the computation. Otherwise, it calculates the inverse of the matirx and
## sets the values of the matrix inverse in the cache via the setMatrix function

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached Matrix Inverse")
                return(m)
        }
        data <- x$getMatrix()
        m <- solve(data, ...)
        x$setInverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}

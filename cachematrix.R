## The goal of these 2 functions that are used to create an object (a list)
## which stores a numeric matrix and cache's it's inverse.

## The first function, makeCacheMatrix, is a function which creates a list
## containing a function which a) sets the value of the matrix, b) gets the 
## value of the matrix, c) sets the value of the inverse, and 
## d) gets the value of inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This next function, cacheSolve, calculates the inverse of the matrix stored
## by makeCacheMatrix, the function above. It first checks to see if the inverse
## has been calculated already -- if so, it gets the inverse from the cache. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse
## via the setinv function.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}

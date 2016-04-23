## The following functions create a special object that stores a 
## matrix and caches its inverse, assuming the matrix is invertible.

## Creates a list containing a function to get a matrix value, set the
## matrix value, get the matrix inverse and set the the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Checks to see if the inverse of the matrix is cached - returns
## cached inverse if available, otherwise calculates the inverse

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
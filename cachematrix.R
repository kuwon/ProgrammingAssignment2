## Put comments here that give an overall description of what your
## functions do
## you can use these functions like this. For example
## 
## a <- matrix(1:4,2,2)
## cacheMatrix <- makeCacheMatrix(a)
## returnValue <- cacheSolve(cacheMatrix)
##
## you can call 'cacheSolve' repeatedly, to see the message it is from cache.

## create four functions. stores them in another environment
## set: set value of input matrix
## get: get value of it
## setInverse: set inverse matrix of input matrix
## getInverse: get value of setInverse 

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
    set <- function(y) {
            x <<- y
            m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## return the inverse of matrix using cache
## first, check the cache matrix.
## if it exists, return cache data
## else call function 'solve' to get inverse of 'x'

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
            message("getting cached data")
            return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}


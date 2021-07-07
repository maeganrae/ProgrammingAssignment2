## Put comments here that give an overall description of what your
## functions do

## The following function calculates the mean of the special “vector” created with the above function.
## But it first checks to see if the mean has already been calculated. It gets the mean from the cache and
## skips the computation. Otherwise, it calculates the mean data and sets the value of the mean in the cache
## through the setMean function.

## Write a short comment describing this function

## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        j <- NULL
        set <- function(y){
                x <<- y
                j <<- NULL
                }
        get <- function() x
                setInverse <- function(inverse) j <<- inverse
                getInverse <- function() j
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix. If the
## inverse has already been calculated, then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        j <- x$getInverse()
        if(!is.null(j)){
                message("Getting cached data")
                return(j)
                }
        mat <- x$get()
        j <- solve(mat,...)
        x$setInverse(j)
        j
}

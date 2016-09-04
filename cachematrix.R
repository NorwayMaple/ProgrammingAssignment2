## Author: NorwayMaple
## Course: Coursera: Johns Hopkins R Programming
## Date: Sunday, September 4, 2016
## Assignment: Progamming Assignment 2: Lexical Scoping

## Contents: Two functions designed to create an object containing a matrix
## and possibly its cached inverse and to caluculate its inverse using
## solve or the cache.

## makeCacheMatrix creates an object containing both a matrix and the cache
## of its inverse if it has been calculated previously.

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
             setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve takes an object created by makeCacheMatrix and returns either
## the cached inverse of the matrix within the object, or if there is no cache,
## calculates the inverse of that matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
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


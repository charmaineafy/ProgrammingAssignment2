##------------------------------------------------------------------------------------
## Module 2	: R Programming
## Assignment 2	: Lexical Scoping - Caching the inverse of a matrix.
## Specification:-
## 1. A pair of functions that compute and cache the inverse of a matrix.
## 2. It is assumes the matrix is a square invertible matrix.
##-----------------------------------------------------------------------------------

## This function creates a matrix object that can cache its inverse, 
## which is a list containing functions to:
## 1. Set the matrix in the cache.
## 2. Get the matrix.
## 3. Set the inverse matrix in the cache.
## 4. Get the inverse matrix.

makeCacheMatrix <- function( m = matrix() ) {

    ## Initialize the inverse matrix.
    i <- NULL

    ## Set the matrix in the cache.
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    ## Get the matrix.
    get <- function() {
     	m
    }

    ## Set the inverse matrix.
    setInverse <- function(inverse) {
        i <<- inverse
    }

    ## Method to get the inverse of the matrix
    getInverse <- function() {
        ## Return the inverse property
        i
    }

    ## Return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of a matrix returned by the above function "makeCacheMatrix".
## If the inverse has already been calculated (and the matrix has not changed),
## then "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

    ## Get the inverse matrix in the list 'x'.
    m <- x$getInverse()

    ## Return the inverse if it is cached.
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    ## Get the matrix.
    data <- x$get()

    ## Compute the inverse matrix.
    m <- solve(data)

    ## Set the inverse to the list 'x'.
    x$setInverse(m)

    ## Return the inverse matrix.
    m
}

## Functions to set, retrieve, and calculate the inverse of a reversible matrix. 
## cacheSolve requires makeCacheMatrix to be run on the dataset first and cacheSolve may be run on that result
## Based on code for caching the mean of a vector from R Programming: Programming Assignment 2 by:
## by Roger D. Peng, PhD, Jeff Leek, PhD, Brian Caffo, PhD


## Methods to cache, set, and retrieve the inverse of a matrix
makeCacheMatrix <- function(x = matrix(), ...) {
                m <- NULL    ## declare a placeholder for our result
                set <- function(y) {
                        x <<- y  ## assign the local matrix, y, to a global matrix, x
                        m <<- NULL 
                }
                get <- function() x  ## return x, the original matrix
                setinverse <- function(solve) m <<- solve  ## solve for m, the inverse of x, and make it globally available
                getinverse <- function() m  ## return m
                
                ## Return a list of available methods, so they may be called in other functions
                list(set = set, get = get,   
                     setinverse = setinverse,
                     getinverse = getinverse)
        }



## Retrieve or compute the matrix inverse
cacheSolve <- function(x = matrix(), ...) {
        
        m <- x$getinverse()  ## retrieve the existing cached value of the inverse matrix utilizing makeCacheMatrix
        
        ## if the cached inverse exists, return it
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## if the inverse does not exist in the cache, solve for it 
        data <- x$get()
        m <- solve(data, ...)
        
        ## cache the inverse before returning it
        x$setinverse(m)
        m
}

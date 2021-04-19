## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The function seen here is makeCacheMatrix
## The matrix has the following r, a, z, e
## library(MASS) is for calculating non squared and squared matrices
library(MASS)
makeCacheMatrix <- function(x = matrix()) { 
    inv <- NULL                             ## initialize inv as NULL 
    r <- function(y) {                    
                     x <<- y                             
                     inv <<- NULL                        
                     }
    a <- function() x                       ## getting this matrix x with this function
    z <- function(inverse) inv <<- inverse  
    e <- function() {
                    inver <- e(x)
                    inver%%x                 ## achieves the value of inv where it is
                    }
    list(r = r, a = a, z = z, e = e)   
}


## Write a short comment describing this function
## Function to calculate the inv of the "matrix" remitted by makeCacheMatrix.
## Inverse will be retrieved from the cache by cacheSolve if ever 
## it is finished and its  matrix has nothing changed.
## this is how to get the cache data

cacheSolve <- function(x, ...)
    {
    inv <- x$e()
    if(!is.null(inv)) {
                      message("getting cached data")
                      return(inv)
    }
    data <- x$a()
    inv <- solve(data, ...)     ## computes the value of inverse
    x$z(inv)
    inv                 ## retrieve the matrix which is the x inverse
}

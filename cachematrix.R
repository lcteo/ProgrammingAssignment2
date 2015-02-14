## Put comments here that give an overall description of what your
## functions do

# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

## Write a short comment describing this function

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

    invmat <- NULL
    
    set <- function(y) {
        x <<- y
        invmat <<- NULL
    }
    
    get <- function() x
    
    setinv <- function(inv) invmat <<- inv
    getinv <- function() invmat
    
    list(set=set, get=get, 
         setinv=setinv, 
         getinv=getinv)

}

## Write a short comment describing this function

# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinv function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    invmat <- x$getinv()

    if(!is.null(invmat)) {
        message("getting from cached data.")
        return(invmat)
    }

    data <- x$get()
    
    invmat <- solve(data)
    
    x$setinv(invmat)
    
    invmat
    
}

## Testing
## > source("cachematrix.R")
## > x = rbind(c(2, 3), c(3, 2))
## > m = makeCacheMatrix(x)
## > m$get()
## [,1] [,2]
## [1,]    2    3
## [2,]    3    2
## > cacheSolve(m)
## [,1] [,2]
## [1,] -0.4  0.6
## [2,]  0.6 -0.4
## > cacheSolve(m)
## getting from cached data.
## [,1] [,2]
## [1,] -0.4  0.6
## [2,]  0.6 -0.4

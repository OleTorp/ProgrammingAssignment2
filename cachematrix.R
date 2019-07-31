## This pair of functions will provide facilities for constructing a global object for caching an invertible 
## matrix and its inverse


## MakeCacheMatrix, is a function that will construct a list-object providing the input matrix with variables and methods
## for global setting and getting its value and inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL               # i is for holding the inverse
        set <- function(y) {
                x <<- y         # x is updated outside the current environment
                i <<- NULL      # i is updated outside the current environment
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## CacheSolve will access the global cache to check if an inverse of x has already been stored and return it. 
## If not, it will compute the inverse and store it before returning the newly computed value. 

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

## Simple Test:
## ---------------------------------------------------------
## > test <- matrix(c(-1,2,3,4,-1,6,7,8,-1), nrow=3, ncol=3)
## >
## > test
## [,1] [,2] [,3]
## [1,]   -1    4    7
## [2,]    2   -1    8
## [3,]    3    6   -1
## >
## > x <- makeCacheMatrix(test)
## >
## > cacheSolve(x)
## [,1]       [,2]        [,3]
## [1,] -0.18359375  0.1796875  0.15234375
## [2,]  0.10156250 -0.0781250  0.08593750
## [3,]  0.05859375  0.0703125 -0.02734375
## >
## > cacheSolve(x)
## getting cached data
## [,1]       [,2]        [,3]
## [1,] -0.18359375  0.1796875  0.15234375
## [2,]  0.10156250 -0.0781250  0.08593750
## [3,]  0.05859375  0.0703125 -0.02734375
## >
## > x$set(test)
## > cacheSolve(x)
## [,1]       [,2]        [,3]
## [1,] -0.18359375  0.1796875  0.15234375
## [2,]  0.10156250 -0.0781250  0.08593750
## [3,]  0.05859375  0.0703125 -0.02734375
## > 


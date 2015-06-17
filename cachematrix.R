##
## Function: makeCacheMatrix
## Purpose: Holds Matrix data and its inverse.  Acts as a cache for the inverse
## Usage:
##     a. makeCacheMatrix(data)      - setup matrix and functions
##     b. $get():     Get matrix data
##     c. $setinv(inverse):   caches the inverse Matrix
##     d. $getinv():  Returned cached inverse matrix
##

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        
        setinv <- function(matrixsolve) inv <<- matrixsolve
        
        getinv <- function() inv
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

##
## Function: cacheSolve
## Purpose: Solve() enhanced to use cached Inverse data.  
## Usage: cacheSolve(x)     x -> CacheMatrix
##

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}


## 
## Test Results
##
##> c=rbind(c(1, -1/4), c(-1/4, 1)) 
##> c
##      [,1]  [,2]
##[1,]  1.00 -0.25
##[2,] -0.25  1.00
##> x = makeCacheMatrix(c)
##> cacheSolve(x)
##          [,1]      [,2]
##[1,] 1.0666667 0.2666667
##[2,] 0.2666667 1.0666667
##> cacheSolve(x)
##getting cached data
##          [,1]      [,2]
##[1,] 1.0666667 0.2666667
##[2,] 0.2666667 1.0666667
##> x$get() %*% cacheSolve(x)
##getting cached data
##     [,1] [,2]
##[1,]    1    0
##[2,]    0    1
##
##

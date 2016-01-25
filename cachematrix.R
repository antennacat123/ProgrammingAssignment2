## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <-function(x = matrix()) {
    inv_m <- NULL
    set <- function(y) {
           x <<- y
           inv_m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv_m <<- inverse
    getInverse <- function() inv_m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Write a short comment describing this function

casheSolve <-function(x, ...) {
     inv_m <- x$getInverse()
     if (!is.null(inv_m)) {
             message("getting cached data")
             return(inv_m)
    }
    matrix1 <- x$get()
    inv_m <- solve(matrix1, ...)
    x$setInverse(inv_m)
    inv_m
}

## Put comments here that give an overall description of what your
## functions do
## Matrix inversion is sometimes a costly computation and it will give some benefit to
## caching the inverse of a matrix instead compute it repeteadly
## Write a short comment describing this function
##Below are two functions that will create the object that will store it and cache its inverse 
makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set <- function(y) {
   x <<- y
    inv <<- NULL
}
 get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function
##it will cache the inverse created by makeCacheMatrix function if inverse is already present then it will not recompute it 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
                           }
    matrix <- x$get()
    inv <- solve(matrix, ...)
    x$setInverse(inv)
    inv
}

## Put comments here that give an overall description of what your
## functions do
## Matrix inversion is sometimes a costly computation and it will give some benefit to
## caching the inverse of a matrix instead compute it repeteadly
## Write a short comment describing this function
##Below are two functions that will create the object that will store it and cache its inverse 
makeCacheMatrix <- function(x = matrix()) {
##initilize the value to NULL
inv <- NULL
##this fuction is setting the value 
set <- function(y) {
   x <<- y
    inv <<- NULL
}
 ## this function is used to get the value
 get <- function() x
  ##this function is used to set the inverse 
  setInverse <- function(inverse) inv <<- inverse
   ##this function is used to get the inverse
  getInverse <- function() inv
   ##creating the list that will be used in calling the function 
list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function
##it will cache the inverse created by makeCacheMatrix function if inverse is already present then it will not recompute it 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   ## getting the inverse of x using getInverse function
        inv <- x$getInverse(
   ##Checking the value of inv if it is null or not
        if (!is.null(inv)) {
        message("getting cached data")
           ##returning the cached value
        return(inv)
                           }
    ## using the get function to get x and assigning into matrix
    matrix <- x$get()
    ## calculating the inverse if it is not calulated earlier
    inv <- solve(matrix, ...)
    ## setting the value of inverse
    x$setInverse(inv)
    ## sending value of inverse
    inv
}

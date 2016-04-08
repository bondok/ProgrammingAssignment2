## File         : CacheInverse.R
## System       : Assignment 2 - RProgramming Course (Coursera)
## Date         : 08/04/2016
## Author       : Ala Halaseh


## These two functions allow the user to compute and cache the inverse of a matrix.  This technique
## is very useful when an inverse is calculated multiple times in a loop, so the first call, will actually
## compute the inverse of the matrix, but all the next calls will retrieve the cached values.

## makeCacheMatrix: a wrapper to matrix (), to allow caching of inverse
## parameters: x - The matrix to be passed
## return: a list of functions to get/set the matrix and its inverse
makeCacheMatrix <- function (x = matrix()){
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function(){
    x
  }
  setInverse <- function(inverse)
  {
    i <<- inverse
  }
  getInverse <- function()
  {
    i
  }
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve: It tries to retrieve the cached inverse from the instance of makeCacheMatrix passed. If it is null, it is actually computed using solve()
## parameter: x - makeCacheMatrix instance
## return: the inverse of x.
cacheSolve <- function(x, ...) {
  i = x$getInverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
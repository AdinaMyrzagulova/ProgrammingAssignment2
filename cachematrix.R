## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x<<-y
    inverse<<-NULL 
  }
  get <- function(){x}
  setInverse<-function(inversecalculada){inverse<<-inversecalculada}
  getInverse<-function(){inverse}
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
cachesolve <- function(x,...){
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data<-x$get()
  inverse <- solve(data,...)
  x$setInverse(inverse)
  inverse
  #return a matrix that is the inverse of 'x'
}

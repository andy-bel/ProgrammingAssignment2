## Written by andy-bel 
## R class assignment #2 computing inverse of a matrix or pulling it from cache if it was already computed 


## The function below creates a list of 4 functions:
## get - returns matrix X supplied as argument during makeCacheMatrix function call or stored 
##       in makeCacheMatrix's environment by "set" function
## set - creates a null object cache_mat in makeCacheMatrix's environment 
##       and replaces matrix X in makeCacheMatrix's environment with supplied argument
## getcachemat - returns the value of cache_mat object in makeCacheMatrix's environment
## setcachemat - sets object cache_mat in makeCacheMatrix's environment equal to supplied argument

makeCacheMatrix <- function(x = matrix()) {
  cache_mat <- NULL
  set <- function(y) {
    x <<- y
    cache_mat <<- NULL
  }
  get <- function() x
  setcachemat <- function(input_mat) cache_mat <<- input_mat
  getcachemat <- function() cache_mat
  list(set = set, get = get, setcachemat = setcachemat, getcachemat = getcachemat)  
}


## The function below utilizes makeCacheMatrix function above to
## 1. check if cache_mat object in makeCacheMatrix's environment, associated with matrix X 
##    provided during makeCacheMatrix'x call, is not NULL
## 2. if cache_mat is not NULL, return it as function's output
## 3. if chache_mat is NULL, obtain original matrix X provided during makeCacheMatrix's call
##    from makeCacheMatrix's environment, compute its inverse, assign to cache_mat object
##    in makeCacheMatrix's environment, and return it as function's output
## As a result, cacheSolve outputs inverse of matrix X provided during makeCacheMatrix's call
## by either computing it the first time or extracting previously computed result stored in 
## makeCacheMatrix's environment.

cacheSolve <- function(x, ...) {
  im <- x$getcachemat()
  if(!is.null(im)) {
    print("Taking solution from cache:")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setcachemat(im)
  im
}

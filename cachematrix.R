## makeCacheMatrix implements 4 methods that allow cacheSolve to 
## maintain the state of a matrix inversion operation. 

## makeCacheMatrix takes an invertible matrix as an input to construct a 
## CacheMatrix object. This function does NOT test the input matrix for 
## invertability.

## A CacheMatrix object stores 2 matrices:
##
## x: This is the original input matrix used to construct a CacheMatrix object
##
## inverse_matrix: This is the inverted matrix
## 
## makeCacheMatrix implements 4 methods:
##
## set: This method is invoked when creating a new CacheMatrix Object. The 
## input matrix is stored and the inverted matrix is set to NULL
##
## get: This method returns the original input matrix
##
## setinverse: This method allows an inverted matrix to be stored in the cache 
## by the cacheSolve function
##
## getinverse: This method allows an inverted matrix to be returned from the 
## cache
##
## makeCacheMatrix will also return a list of the methods it implements


makeCacheMatrix <- function(x = matrix()) {

  inverse_matrix <- NULL
  
  set <- function(y) {
    x <<- y
    inverse_matrix <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(im) inverse_matrix <<- im
  
  getinverse <- function() inverse_matrix
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## This function takes a CacheMatrix object as input and checks to see if a 
## copy of the inverted matrix exists in the cache and returns the cached 
## matrix if it exists. If a cached matrix does not exist, this function will
## invoke solve() and calculate a new inverted matrix and return it.
## The invertible matrix I used to test this is:
##  0  -3  -2
##  1  -4  -2
## -3   4   1

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inverse_matrix <- x$getinverse()
  
  if(!is.null(inverse_matrix)) {
    message("getting cached data")
    return(inverse_matrix)
  }
  data <- x$get()
  inverse_matrix <- solve(data, ...)
  x$setinverse(inverse_matrix)
  inverse_matrix
  
}

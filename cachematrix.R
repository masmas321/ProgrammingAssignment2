## ------------------------------
##  R-Programming - Assignment 2
## ------------------------------
## The purpose of this Assignment is Caching the 
## Inverse of a Matrix so that it be can be retrieved
## without having to be computed repeatedly.

## The makeCacheMatrix function creates the matrix object
## which can Cache its Inverse. 
## The Inverse of the matrix is computed and cached, 
## if this inverse needs to be called, this can be done by
## calling cacheSolve which would return the cached inverse
## if this inverse has already been computed. This avoids
## the inverse from been computed again and would be a less
## costly operation. 

## The makeCacheMatrix function takes in a Matrix as an 
## argument and creates the matrix object which can 
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {
   
   matrixInverse <- NULL
   
   ## Set the Matrix
   set <- function(y) {
      x <<- y
      matrixInverse <<- NULL
   }
   
   ## Get the Matrix
   get <- function() x
   
   ## Set the Inverse of the Matrix
   setMatrixInverse <- function(inverse) matrixInverse <<- inverse
   
   ## Get the Inverse of the Matrix
   getMatrixInverse <- function() matrixInverse
   
   list(set = set, get = get,
        setMatrixInverse = setMatrixInverse,
        getMatrixInverse = getMatrixInverse)
}


## the cacheSolve Function requires the matrix object 
## from makeCacheMatrix as an argument and computes the 
## inverse. This function retrieves the cached inverse
## if this had already been computed and unchanged. 

cacheSolve <- function(x, ...) {
   
      matrixInverse <- x$getMatrixInverse()
      
      ## if cached inverse is available, return the cached inverse
      if(!is.null(matrixInverse)) {
         message("Retrieving Cached Data")
         return(matrixInverse)
      }
      
      data <- x$get()
      
      matrixInverse <- solve(data, ...)
      
      x$setMatrixInverse(matrixInverse)
      
      return(matrixInverse)
}

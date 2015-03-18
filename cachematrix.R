## -------------------------------------------------------
## Purpose of this file
## -------------------------------------------------------
## This set of functions will enable the caching of the 
## inverse of an invertable matrix by creating a special
## matrix that can persist the value of the inverted 
## message in memory by leveraging lexical scoping

## -------------------------------------------------------
## Order of execution for these two functions
## (with example usage)
## -------------------------------------------------------
## a<-makeCacheMatrix(x)
## cacheSolve(a)
## cacheSolve(a)
## -------------------------------------------------------

## -------------------------------------------------------
## Function: makeCacheMatrix
## -------------------------------------------------------
## This function creates the special matrix by
## accepting a matrix object and returning the
## resulting special matrix.  A special matrix
## has simularities to an object in other languages
## as it bundles both the data, a matrix, along with
## a set of methods to interact with that data.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## -------------------------------------------------------
## Function: cacheSolve
## -------------------------------------------------------
## This function solves for the inverse of the matrix if it
## hasn't already been stored within the matrix.  First it
## will interegate the matrix for a solved matrix by calling
## the matrix's getinverse() function, if it returns a 
## a valid (non-NULL value) matrix then it returns that value
## if that returns NULL
## then it solves the matrix and passes that inverse to the
## matrix to store using the setinverse() function


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinverse(inv)
  inv  
}

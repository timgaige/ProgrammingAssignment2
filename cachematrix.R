## This script contains two functions. One to create a cacheMatrix List and the other to retrieve the
## inverse of that matrix. The matrix inverse will be retrieved from cache otherwise it will compute the
## inverse on the fly using the built-in SOLVE function

## FUNCTION NAME: makeCacheMatrix
## INPUTS: R Object containing a valid square matrix
## OUTPUTS: List object including get / set functions
## USAGE: makeCacheMatrix(myMatrix)

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## FUNCTION NAME: cacheSolve
## INPUTS: Object created from cacheMatrix
## OUTPUTS: Function will check to see if matrix has previously been solved and if so, pull it from cache
##          otherwise it will solve the matrix and store it back into the supplied object
## USAGE: cacheSolve(myCacheMatrix)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("Getting data from cache")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

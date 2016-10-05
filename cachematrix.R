## These functions calculate and cache the inverse of specified matrix.  
## When calculating, these functions will looked for a cached answer prior to running the calculation.

##This function takes in a matrix and returns a list of functions to be made available in the next function

makeCacheMatrix <- function(x = matrix()){
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}

## This function looks for a cached value for the inverse of the matrix provided as the argument in the first function.  If one is found it is returned otherwise it is calculated, cached and returned.

cacheSolve <- function(x, ...){
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
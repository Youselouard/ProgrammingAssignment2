##Two functions allowing to create a matrix with the ability to cache its inverse

## This function creates the special matrix and returns a list functions to set and get the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  ##function to set new value for matrix
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  ##function to get x
  get <- function() x
  
  ##function to set the inverse
  setinv <- function(inv) {
    inverse <<- inv
  }
  ##function to get the inverse
  getinv <- function() inverse
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}


## This function calculates, caches and returns the inverse if it has not been calculated previously 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  } 
  
  matrix <- x$get()
  
  inv <- solve(matrix)
  
  x$setinv(inv)
  
  inv
}

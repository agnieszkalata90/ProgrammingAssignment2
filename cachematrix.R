###################################
##### R Programming COURSERA ######
###################################

######### Assignment 2 ############



## Functions that cache the inverse of a matrix.

## makeCacheMatrix() function creates a special "matrix" object that can cache 
## its inverse which is really a list containing a function to set and get the 
## value of the matrix as well as set and get the value of its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) inv <<- solve
  getsolve <- function() inv
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)   
}

## cacheSolve() function calculates the mean of the special "matrix" created with 
## the above function. First it checks whether the inverse has already been 
## calculated. If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the 
## inverse in the cache via the setmean function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getsolve()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setsolve(inv)
  inv
}
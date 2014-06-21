## typical usage example:
## CachableMatrix<-makeCacheMatrix(matrix(c(9,3,5,-6,-9,7,-1,-8,1), nrow=3, ncol=3, byrow=TRUE))
## cacheSolve(CachableMatrix) Freshly calculated inverse
## cacheSolve(CachableMatrix) Stored inverse
## CachableMatrix$set(matrix(c(8,2,6,-7,-8,7,-1,-5,1), nrow=3, ncol=3, byrow=TRUE))
## cacheSolve(CachableMatrix) Freshly calculated inverse
## 

## First run of cacheSolve will preform solve on 'x'and save the 
## result into 'm' of makeCacheMatrix variable
## Each additional run returns the stroed result
## This allows repeat runs to take signifigantly less time
## Anytime set is called on the return from makeCacheMatrix the 
## stored inverse matrix will be set to null and force a recalculation
## on the following cacheSolve run.


## makeCacheMatrix function accepts a matrix and returns a list of 4 
## functions 
##		1) set(y) 'y' must be a matrix, Sets 'x' to 'y' and 'm' to null  
## 		2) get() returns matrix stored in 'x' in the enviroment 
##				 that created the function list
##		3) setsolve(solve) stores inverse matrix into 'm'
##		4) getsolve() returns stored inverse matrix from 'm'

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve will return the inverse of 'x'
## 'x' must have been created through makeCacheMatrix function above
## '...' argument is passed directly to solve function and can be any legitimate value of the solve function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

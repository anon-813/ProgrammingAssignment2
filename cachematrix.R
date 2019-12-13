cat("\014")  
rm(list = ls())



## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## this function takes a square matrix and 
  ## returns a list that is used as the input to cacheSolve()
  
  inv = NULL
  set = function(y) {
    x <<- y  
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}



cacheSolve <- function(x, ...) {
  ## returns the inverse of the original matrix input to makeCacheMatrix()
  
  inv = x$getinv()

  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }

  m = x$get() 
  inv = solve(m, ...)
  
  x$setinv(inv)
  
  return(inv)
}






########## Testing

A <- as.matrix(cbind(c(3,0),c(1,2)))
A

B<- makeCacheMatrix(A)


cacheSolve(B)

solve(A)


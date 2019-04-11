## first function calculate and store the inverse of a function assuming the function is invertible, the second 
#install.packages("matlib")
library(matlib)

## calculate and store the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## check if the inverse has been previously calculated, if yes display the stored result
cacheSolve <- function(x, ...) {
  m<-x$getinverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
} #return a matrix which is the inverse of x

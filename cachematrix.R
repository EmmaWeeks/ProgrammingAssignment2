## these two functions, when used as cacheSolve(makeCacheMatrix(X)),
## output the inverse of matrix X. Once the inverse has been calculated
## the inverse matrix is cached, and can be retrieved later without
## having to recalculate, which with a large matrix will save a lot of 
## time and effort

## this function takes a matrix and outputs a list of functions to be
## used by cacheSolve
makeCacheMatrix <- function(x = matrix()) {

  s <- NULL
     set <- function (y) {
      x <<- y
      s <<- NULL
     }
     get <-function() x
     setinverse <- function(solve) s <<- solve
     getinverse <- function() s
     list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}




## this takes makeCacheMatrix and outputs the inverse, either by
## calculating it from scratch or the cached value
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getinverse()
     if(!is.null(s)) {
      message("getting cached data")
      return(s)
     }
     data <- x$get()
     s <- solve(data, ...)
     x$setinverse(s)
     s
}

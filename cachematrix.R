##This function acts analogously to the example makeVector function and 
##creates a list containing a function to:
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse matrix
g##et the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverseMatrix <<- inverse
  getinverse <- function() inverseMatrix
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
# The following function acts similarly to the cachemean example function and
#returns the inverse of the matrix (assuming that the matrix
#is always invertible). If the inverse has already been computed, it returns the stored
#inverse, otherwise it computes and stores the inverse in the cache.
cacheSolve <- function(x, ...) {
  inverseMatrix <- x$getinverse()
  if(!is.null(inverseMatrix)) {
    message("getting cached data.")
    return(inverseMatrix)
  }
  data <- x$get()
  inverseMatrix <- solve(data) 
  x$setinverse(inverseMatrix)
  inverseMatrix
}

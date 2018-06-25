## The combination of functions below can be used to compute the inverse of a matrix, and to get the outcome from cache
## if it's already been calculated. Use the makeCacheMatrix function to convert the matrix to a form that's
## 'cachable'. Then use the cacheSolve function on the converted matrix (technically a list) to compute the inverse,
## or get the inverse from cache if it has already been calculated previously.

## This function creates a list containing a function to set the value of the matrix, get the value of the matrix, 
## set the matrix' inverse, and get the matrix' inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## The following function calculates the inverse of the special "matrix" created with the above function. 
## It first checks to see if the inverse has already been calculated. If so, it gets the inverse from the 
## cache and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value of the 
## inverse matrix in the cache via the setinverse function.

cacheSolve<- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}


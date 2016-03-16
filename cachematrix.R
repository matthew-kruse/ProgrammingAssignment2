## These functions calculate the inverse of a square matrix 
## with the ability to cache results

## This functions prepares a matrix inverse calculation to be cached
## by creating a list of functions to set, get, setinverse, getinverse
## of a square matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## this function computs a matrix inverse or pulls from cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data for matrix inverse")
    return(m)
  }
  else {
    message("computing matrix inverse")
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
  }
}

## TO RUN
# tmp <- makeCacheMatrix(matrix(c(2, 4, 3, 1), 2, 2))
# cacheSolve(tmp)
# cacheSolve(tmp)

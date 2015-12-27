##makeCacheMatrix function creates a special matrix.
## It creates a list of functions to set, get the inverse matrix calculated with cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## ## This function Returns a matrix that is the inverse of a given 'x' matrix
#If the inverse of the matrix has already been calculated, the funtion gets cached data.
#Ifnot the function calculates the inverse matrix.

cacheSolve <- function(x, ...) {
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("Getting cached data...")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
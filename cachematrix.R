## Two functions that cache the inverse of a matrix
## if it is not already cached.

## makeCacheMatrix
## Allows you to set and get the value of the matrix
## Also allows you to set and get the inverse
## Contains a list containing those functions

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setmatrix <- function(matrixInverse) m <<- matrixInverse
  
  getmatrix <- function() m
  
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## Check to see if inverse has already been cached.
## Gets inverse from cache if it is not NULL
## Otherwise calculates inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}

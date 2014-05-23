## Put comments here that give an overall description of what your
## functions do

## This function creates a cached matrix
## This matrix contains a cached version of its inverse
makeCacheMatrix <- function(x = matrix()) {
  # The inverse Matrix
  inv <- NULL
  
  # Set function
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # Get function
  get <- function() x
  
  # Inverse matrix set function
  setinverse <- function(inverse) inv <<- inverse
  
  # Inverse matrix get function
  getinverse <- function() inv
  getinverse
  
  # Return the matrix object
  list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Solves (computes the inverse of) a cached matrix 
## caching the value for future use
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # Compute the inverse
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  
  # Return the inverse
  m
}

# An example
matTest = matrix(c(10,1,-41,2,-2,0,30,-3,-20),3,3)
cachedMatrix <- makeCacheMatrix(matTest)
cacheSolve(cachedMatrix)
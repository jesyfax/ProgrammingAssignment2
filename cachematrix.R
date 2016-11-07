#This function creates a special "matrix" object that create a matrix and can cache its inverse which returns a list of function to 
#setMatrix
#getMatrix
#cacheInverse
#getInverse

makeCacheMatrix <- function(x = matrix()) {
  
  # initially nothing is cached so set it to NULL
  cache <- NULL
  
  # set a matrix
  setMatrix <- function(newValue) {
    x <<- newValue
    # since the matrix is assigned a new value, delete the cache
    cache <<- NULL
  }
  
  # returns the created matrix
  getMatrix <- function() {
    x
  }
  
  # cache the argument 
  cacheInverse <- function(solve) {
    cache <<- solve
  }
  
  # get the cache values
  getInverse <- function() {
    cache
  }
  
  # return a list of functions
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


#---------------------------------------------------------------------------------------------------
# gives the inverse of the matrix created with makeCacheMatrix
cacheSolve <- function(y, ...) {
  # get the cache value
  inverse <- y$getInverse()
  # if a cached value exists return it
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  # otherwise get the matrix, get the inverse and store it in the cache
  data <- y$getMatrix()
  inverse <- solve(data)
  y$cacheInverse(inverse)
  
  # return the inverse
  inverse
}


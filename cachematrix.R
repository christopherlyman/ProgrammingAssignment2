## R Programming Assignment 2: Lexical Scoping
## Assignment: Caching the Inverse of a Matrix

# makeCacheMatrix does the following:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of the matrix
# 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      
      # 1. Set the value of the special matrix
      setMatrix <- function(y) {
            x <<- y
            m <<- NULL
      }
      
      # 2. get the value of the special matrix
      getMatrix <- function() x
      
      # 3. set the value of the inverse of the special matrix
      setInverse <- function(solve) m <<- solve
      
      # 4. get the value of the inverse of the special matrix
      getInverse <- function() m
      
      list(set = setMatrix, getMatrix = getMatrix,
           setInverse = setInverse,
           getInverse = getInverse)
}



# The cacheSolve function calculates the inverse of
# the special "vector" created with makeCacheMatrix 
# function. 
# First, it checks to see if the inverse has already 
# been calculated. If so, it gets the inverse from the 
# cache and skips the computation. Otherwise, it 
# calculates the inverse of the data and sets the 
# value of the inverse in the cache via the setInverse 
# function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      
      # assign cached value.
      m <- x$getInverse()
      
      # check for valid cache value.
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      
      # If cached value is NULL assign special matrix
      data <- x$getMatrix()
      
      # take inverse of special matrix
      m <- solve(data, ...)
      
      # set inverse of special matrix as cached value.
      x$setInverse(m)
      m
}

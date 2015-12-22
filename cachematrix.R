## Provides a container for a matrix and its inverse. This allows the inverse
## to be cached, saving computational time (should the inverse be needed often)

## Returns a list of a matrix, its inverse, and functions to set and get each
makeCacheMatrix <- function(mtrx = matrix()) {
  inv <- NULL #inverse is, by default, NULL
  
  #When an existing cacheMatrix is altered, the inverse is reset to NULL
  set <- function(y){
    mtrx <<- y
    inv <<- NULL
  }
  
  #return the matrix itself
  get <- function() mtrx
  
  #set the inverse
  setinverse <- function(inverse) inv <<- inverse
  
  #return the inverse of the matrix
  getinverse <- function() inv
  
  #return the list of all variables constructed herein
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


##Checks if the inverse of the matrix has already been calculated.
##If so, it returns the cached inverse.
##If not, it calculates and stores the inverse as the inv variable
##of the cacheMatrix.
cacheSolve <- function(cmtrx, ...) {
  inv <- cmtrx$getinverse() #get the cached inverse, if present
  
  #if the cached inverse is not present (inv is null), 
  #then calculate and cache the inverse.
  if(is.null(inv)){
    message("calculating...")
    data <- cmtrx$get()
    inv <- solve(data)
    cmtrx$setinverse(inv)
  }
  
  inv
}
